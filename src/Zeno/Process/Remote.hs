
module Zeno.Process.Remote where

import Control.Concurrent.STM (throwSTM)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.FixedBytes
import Data.Hashable
import Data.Binary
import Data.Typeable
import Data.Void
import GHC.Generics (Generic)

import qualified Network.Transport as NT
import qualified StmContainers.Map as STM
import UnliftIO hiding (Chan)

import UnliftIO.Concurrent
import Unsafe.Coerce

import Zeno.Process.Types
import Zeno.Prelude hiding (finally)


sendRemote :: (Binary a, Has Node r) => NodeId -> ProcessId -> a -> Zeno r ()
sendRemote nodeId pid msg = do
  chan <- getRemoteForwarder nodeId
  atomically $ writeTQueue chan $ Forward $ encode (pid, msg)


monitorRemote :: Has Node r => NodeId -> Zeno r () -> Zeno r ()
monitorRemote nodeId act = do
  chan <- getRemoteForwarder nodeId
  ioAct <- toIO act
  atomically $ writeTQueue chan $ OnShutdown $ ioAct


getRemoteForwarder :: Has Node r => NodeId -> Zeno r Forwarder
getRemoteForwarder nodeId = do
  node@Node{mforwarders} <- asks has

  -- TODO: Mask
  
  (chan, created) <- atomically do
    STM.lookup nodeId mforwarders >>=
      \case
        Just chan -> pure (chan, False)
        Nothing -> do
          chan <- newTQueue
          STM.insert chan nodeId mforwarders
          pure (chan, True)

  when created do
    forkForwarder node nodeId chan
    pure ()
  pure chan

quitRemoteForwader :: Node -> NodeId -> STM ()
quitRemoteForwader Node{..} nodeId = do
  STM.lookup nodeId mforwarders >>=
    mapM_ \fwd -> do
      writeTQueue fwd Quit
      STM.delete nodeId mforwarders

forkForwarder :: Node -> NodeId -> TQueue ForwardMessage -> Zeno r ThreadId
forkForwarder node@Node{..} nodeId chan = do
  forkIOWithUnmask \restore -> finally (restore run) cleanup
  where
  run :: Zeno r ()
  run = do
    liftIO mkConn >>=
      either warnDidNotSend
        \conn -> do
          liftIO (NT.send conn [""]) >>=
            either warnDidNotSend (\() -> loopForward conn)
            -- TODO: finally close conn
  
  mkConn :: IO (Either (NT.TransportError NT.ConnectErrorCode) NT.Connection)
  mkConn = NT.connect endpoint (endpointAddress nodeId) NT.ReliableOrdered NT.defaultConnectHints

  loopForward :: NT.Connection -> Zeno r ()
  loopForward conn = do
    fix1 [] $ \f handlers -> do
      atomically (readTQueue chan) >>=
        \case
          Forward msg -> do
            liftIO $ NT.send conn $ BSL.toChunks msg
            f handlers
          OnShutdown act -> f $ act : handlers
          Quit -> do
            liftIO $
              sequence_ handlers

  cleanup = do
    atomically do
      STM.lookup nodeId mforwarders >>=
        mapM_ \chan' -> do
          when (chan' == chan) do
            writeTQueue chan Quit
            STM.delete nodeId mforwarders

  warnDidNotSend :: Show e => e -> Zeno r ()
  warnDidNotSend err = do
    logWarn $ "Forwarded: Error setting up lightweight connection: " ++ show err


subscribe :: (Has Node r, Binary i) => ProcessId -> Zeno r (RemoteReceiver i)
subscribe procId = do
  node@Node{..} <- asks has
  (_, recv) <- 
    allocate
      (atomically $ getReceiverSTM node procId)
      (\_ -> atomically $ STM.delete procId topics)
  pure recv

getReceiverSTM :: Binary i => Node -> ProcessId -> STM (RemoteReceiver i)
getReceiverSTM Node{..} pid = do
  STM.lookup pid topics >>=
    \case
      Just r -> throwSTM TopicIsRegistered
      Nothing -> do
        recv <- newTQueue
        STM.insert (wrap recv) pid topics
        pure recv
  where
  wrap chan = WrappedReceiver $
    \nodeId bs -> do
      case decodeOrFail bs of
        Right ("", _, a) -> atomically $ writeTQueue chan $ RemoteMessage nodeId a
        Right (_, _, _) -> pure () -- TODO: log
        Left _ -> pure () -- TODO: log

withRemoteMessage :: (NodeId -> a -> m b) -> RemoteMessage a -> m b
withRemoteMessage act (RemoteMessage nodeId msg) = act nodeId msg
