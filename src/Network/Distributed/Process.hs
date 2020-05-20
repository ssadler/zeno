
module Network.Distributed.Process where


import Data.Dynamic
import Data.FixedBytes
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import UnliftIO
import qualified StmContainers.Map as STM

import Network.Distributed.Types

import Crypto.Hash



getProcessById :: Node -> ProcessId -> STM (Maybe ProcessHandle)
getProcessById Node{..} pid = do
  STM.lookup pid processes


getMyPid :: Process p => p ProcessId
getMyPid = procAsks myPid


newPid :: Node -> STM ProcessId
newPid node@Node{..} = do
  lp <- readTVar lastPid
  let np = blake2b_160 (salt <> lp)
  writeTVar lastPid np
  pure $ ProcessId $ toFixed np



nodeSpawn :: ProcessBase m => Node -> RunProcess m -> m ProcessHandle
nodeSpawn node act = nodeSpawn' node act

nodeSpawn' :: ProcessBase m => Node -> RunProcess m -> m ProcessHandle
nodeSpawn' node act = do
  pid <- atomically $ newPid node
  nodeSpawnNamed node pid act


nodeSpawnNamed :: ProcessBase m => Node -> ProcessId -> RunProcess m -> m ProcessHandle
nodeSpawnNamed node@Node{processes} pid act = do
  atomically (STM.lookup pid processes) >>=
    maybe (pure ()) (\_ -> throwIO ProcessNameConflict)

  chan <- newTQueueIO
  handoff <- newEmptyTMVarIO

  async' <- async do
    r <- ProcData node pid chan <$> atomically (takeTMVar handoff)
    finally (act r) do
      atomically $ STM.delete pid processes

  let proc = Proc chan async' pid

  atomically do
    STM.insert proc pid processes
    putTMVar handoff async'

  pure proc


spawn :: Process p => p () -> p ProcessHandle
spawn act = do
  procAsks myNode >>= \n -> nodeSpawn n (flip procWith act)


spawnNamed :: Process p => ProcessId -> p () -> p ProcessHandle
spawnNamed pid act = do
  procAsks myNode >>= \node -> nodeSpawnNamed node pid (flip procWith act)



killProcess :: MonadIO m => Node -> ProcessId -> m ()
killProcess node pid = do
  atomically (getProcessById node pid) >>=
    \case
      Nothing -> pure ()
      Just Proc{..} -> uninterruptibleCancel procAsync


sendSTM :: Typeable a => Node -> ProcessId -> a -> STM ()
sendSTM node pid msg = do
  getProcessById node pid >>=
    \case
      Nothing -> pure ()
      Just proc -> sendProcSTM proc msg


sendProcSTM :: Typeable a => ProcessHandle -> a -> STM ()
sendProcSTM Proc{..} = writeTQueue procChan . toDyn


send :: (Process p, Typeable a) => ProcessId -> a -> p ()
send pid msg = do
  node <- procAsks myNode
  atomically $ sendSTM node pid msg



receiveWait :: (Process p, Typeable a) => p a
receiveWait = do
  r <- procAsks inbox >>= atomically . readTQueue
  case fromDynamic r of
    Nothing -> liftIO (print "receiveWait miss") >> receiveWait
    Just a -> pure a

blake2b_160 :: BS.ByteString -> BS.ByteString
blake2b_160 b = BS.pack (BA.unpack (hash b :: Digest Blake2b_160))


serviceId :: BS.ByteString -> ProcessId
serviceId = ProcessId . toFixed . blake2b_160


