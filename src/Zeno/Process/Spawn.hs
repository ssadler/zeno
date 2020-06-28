
module Zeno.Process.Spawn where

import qualified Data.ByteString as BS
import Data.Time.Clock

import UnliftIO
import UnliftIO.Async
import UnliftIO.STM

import Zeno.Process.Types
import Zeno.Prelude

type MonadBase m = (MonadUnliftIO m, MonadResource m, MonadLogger m)


spawn :: forall i r b. String -> (AsyncProcess i b -> Zeno r b) -> Zeno r (AsyncProcess i b)
spawn threadName forked = do
  handoff <- newEmptyMVar
  procMbox <- newTBQueueIO 3
  UnliftIO unliftIO <- askUnliftIO

  let
    debugThreads = False

    runThread = do
      when debugThreads do
        -- traceM $ emot emSnout ++ " : " ++ threadName
        atomically (modifyTVar globalThreadCount (+1))
      asyncOn 0 do
        unliftIO do
          withLocalResources do
            logDiedSync threadName $ readMVar handoff >>= forked

    stopThread asnc = do
      cancel asnc
      when debugThreads do
        -- traceM $ emot emFrogFace ++ " : " ++ threadName
        atomically (modifyTVar globalThreadCount (+(-1)))
        readTVarIO globalThreadCount >>= print

  -- Using allocate and checkpointing the resourceT state is how heirarchical
  -- thread lifetimes are achieved
  (_, procAsync) <- allocate runThread stopThread
  let proc = Process{..}
  putMVar handoff proc
  pure proc

logDiedSync :: String -> Zeno r a -> Zeno r a
logDiedSync threadName act = do
  catchAny act $ \e -> do
    logError $ "Thread \"%s\" died with: %s" % (threadName, show e)
    throwIO (e :: SomeException)

spawnNoHandle :: forall i r b. String -> Zeno r b -> Zeno r ()
spawnNoHandle label act = void $ spawn label \_ -> act

globalThreadCount :: TVar Int
globalThreadCount = unsafePerformIO $ newTVarIO 0

send :: MonadIO m => AsyncProcess i b -> i -> m ()
send proc i = atomically $ sendSTM proc i

sendSTM :: AsyncProcess i b -> i -> STM ()
sendSTM Process{..} = writeTBQueue procMbox

request :: MonadIO m => AsyncProcess i b -> (MVar o -> i) -> m o
request proc f = do
  m <- newEmptyMVar
  send proc $ f m
  takeMVar m

receiveWait :: (HasReceive r i, MonadIO m) => r -> m i
receiveWait = atomically . receiveSTM

receiveMaybe :: (MonadIO m, Typeable i, HasReceive r i) => r -> m (Maybe i)
receiveMaybe = atomically . receiveMaybeSTM

receiveTimeout :: (MonadIO m, HasReceive r i) => r -> Int -> m (Maybe i)
receiveTimeout recv us = timeoutSTM us $ receiveSTM recv

receiveTimeoutS :: (MonadIO m, Typeable i) => Receiver i -> Int -> m (Maybe i)
receiveTimeoutS recv = receiveTimeout recv . (* second)

receiveDuring :: (HasReceive r i, MonadIO m) => r -> Int -> (i -> m ()) -> m ()
receiveDuring recv timeout act = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    d <- liftIO $ timeDelta startTime
    let us = timeout - d
    receiveTimeout recv us >>= mapM_ act
    when (us > 0) f

receiveDuringS :: (MonadBase m, Typeable i) => Receiver i -> Int -> (i -> m ()) -> m ()
receiveDuringS recv = receiveDuring recv . (* second)
  
second :: Num i => i
second = 1000000
