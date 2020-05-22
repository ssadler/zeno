
module Network.Distributed.Process where


import Crypto.Hash

import Data.Void
import Data.Typeable
import Data.FixedBytes
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified StmContainers.Map as STM
import Control.Monad.Reader

import Network.Distributed.Types

import UnliftIO
import UnliftIO.Exception

import Unsafe.Coerce

import Debug.Trace


getVoidProcessById :: Node -> ProcessId -> STM (Maybe (ProcessHandle Void))
getVoidProcessById Node{..} pid = STM.lookup pid processes


getProcessById :: Typeable i => Node -> ProcessId -> STM (Maybe (ProcessHandle i))
getProcessById node pid = do
  getVoidProcessById node pid >>=
    \case
      Nothing -> pure Nothing
      Just wph -> pure $ Just $ safeCoerceProcessHandle wph


safeCoerceProcessHandle :: forall i. Typeable i => ProcessHandle Void -> ProcessHandle i
safeCoerceProcessHandle proc = do
  if procQType proc == typeRep (Proxy :: Proxy i)
     then unsafeCoerce proc
     else error "Cannot coerce queue"


getMyPid :: MonadProcess i m p => p i m ProcessId
getMyPid = procAsks myPid


newPid :: Node -> STM ProcessId
newPid node@Node{..} = do
  lp <- readTVar lastPid
  let np = blake2b_160 (salt <> lp)
  writeTVar lastPid np
  pure $ ProcessId $ toFixed np



nodeSpawn :: (Typeable i, ProcessBase m) => Node -> RunProcess i m -> m (ProcessHandle i)
nodeSpawn node act = nodeSpawn' node act

nodeSpawn' :: (Typeable i, ProcessBase m) => Node -> RunProcess i m -> m (ProcessHandle i)
nodeSpawn' node act = do
  pid <- atomically $ newPid node
  nodeSpawnNamed node pid act


nodeSpawnNamed :: forall i m. (Typeable i, ProcessBase m)
               => Node -> ProcessId -> RunProcess i m -> m (ProcessHandle i)
nodeSpawnNamed node@Node{processes} pid act = do
  atomically (STM.lookup pid processes) >>=
    maybe (pure ()) (\_ -> throwIO ProcessNameConflict)

  chan <- newTQueueIO
  handoff <- newEmptyTMVarIO
  let rep = typeRep (Proxy :: Proxy i)

  async' <- async do
    r <- ProcData node pid chan rep <$> atomically (takeTMVar handoff)
    finally (catchAny (act r) traceShowM) do
      atomically $ STM.delete pid processes

  let proc = Proc chan rep async' pid
      vproc = unsafeCoerce proc

  atomically do
    STM.insert vproc pid processes
    putTMVar handoff async'

  pure proc


spawn :: (MonadProcess i m p, MonadProcess i2 m p2) => p2 i2 m () -> p i m (ProcessHandle i2)
spawn act = do
  procAsks myNode >>= \n -> procLift (nodeSpawn n (procRun act))


spawnNamed :: (MonadProcess i m p, MonadProcess i2 m p2) => ProcessId -> p2 i2 m () -> p i m (ProcessHandle i2)
spawnNamed pid act = do
  procAsks myNode >>= \node -> procLift (nodeSpawnNamed node pid (procRun act))


monitorLocal :: ForkProcess p i Void m => ProcessId -> p Void m () -> p i m ()
monitorLocal pid act = do
  node <- procAsks myNode
  atomically (getVoidProcessById node pid) >>=
    \case
      Nothing -> pure ()
      Just Proc{..} -> do
        spawn do
          waitCatch procAsync >>= \e -> act
        pure ()


killProcess :: MonadIO m => Node -> ProcessId -> m ()
killProcess node pid = do
  atomically (getVoidProcessById node pid) >>=
    \case
      Nothing -> pure ()
      Just Proc{..} -> uninterruptibleCancel procAsync


sendSTM :: Typeable i => Node -> ProcessId -> i -> STM ()
sendSTM node pid msg = do
  getProcessById node pid >>=
    \case
      Nothing -> pure ()
      Just proc -> sendProcSTM proc msg


sendProcSTM :: Typeable i => ProcessHandle i -> i -> STM ()
sendProcSTM Proc{..} = writeTQueue procChan


send :: (MonadProcess i m p, Typeable a) => ProcessId -> a -> p i m ()
send pid msg = do
  node <- procAsks myNode
  atomically $ sendSTM node pid msg


receiveWait :: MonadProcess i m p => p i m i
receiveWait = do
 procAsks id >>= atomically . receiveWaitSTM


receiveWaitSTM :: ProcessData i -> STM i
receiveWaitSTM ProcData{..} = readTQueue inbox
{-# INLINE receiveWaitSTM #-}



receiveMaybeSTM :: Typeable i => (ProcessData i) -> STM (Maybe i)
receiveMaybeSTM ProcData{..} = tryReadTQueue inbox


receiveMaybe :: MonadProcess i m p => p i m (Maybe i)
receiveMaybe = do
  procAsks id >>= atomically . receiveMaybeSTM

blake2b_160 :: BS.ByteString -> BS.ByteString
blake2b_160 b = BS.pack (BA.unpack (hash b :: Digest Blake2b_160))


serviceId :: BS.ByteString -> ProcessId
serviceId = ProcessId . toFixed . blake2b_160


