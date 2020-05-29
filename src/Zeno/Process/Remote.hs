
module Zeno.Process.Remote where

import Control.Concurrent.STM (throwSTM)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Zeno.Data.FixedBytes
import Data.Hashable
import Data.Serialize
import Data.Typeable
import Data.Void
import GHC.Generics (Generic)

import qualified Network.Transport as NT
import qualified StmContainers.Map as STM
import UnliftIO hiding (Chan)

import UnliftIO.Concurrent
import Unsafe.Coerce

import Zeno.Process.Types
import Zeno.Process.Node.ReceiveMissCache
import Zeno.Prelude hiding (finally)


sendRemote :: (Serialize a, Has Node r) => NodeId -> ProcessId -> a -> Zeno r ()
sendRemote nodeId pid msg = do
  node <- asks has
  chan <- getRemoteForwarder node nodeId
  atomically $ writeTQueue chan $ Forward $ encodeLazy (pid, msg)


monitorRemote :: Has Node r => NodeId -> Zeno r () -> Zeno r ()
monitorRemote nodeId act = do
  node <- asks has
  chan <- getRemoteForwarder node nodeId
  ioAct <- toIO act
  atomically $ writeTQueue chan $ OnShutdown $ ioAct


getRemoteForwarder :: Node -> NodeId -> Zeno r Forwarder
getRemoteForwarder node@Node{mforwarders} nodeId = do
  mask \restore -> do
    (chan, created) <- getChan
    when created do
      let run = runForwarder node nodeId chan
      forkIO (restore run `finally` cleanup chan)
      pure ()
    pure chan

  where
  getChan =
    atomically do
      STM.lookup nodeId mforwarders >>=
        \case
          Just chan -> pure (chan, False)
          Nothing -> do
            chan <- newTQueue
            STM.insert chan nodeId mforwarders
            pure (chan, True)

  cleanup chan = do
    atomically do
      STM.lookup nodeId mforwarders >>=
        mapM_ \chan' -> do
          when (chan' == chan) do
            writeTQueue chan Quit
            STM.delete nodeId mforwarders

quitRemoteForwader :: Node -> NodeId -> STM ()
quitRemoteForwader Node{..} nodeId = do
  STM.lookup nodeId mforwarders >>=
    mapM_ \fwd -> do
      writeTQueue fwd Quit
      STM.delete nodeId mforwarders

runForwarder :: Node -> NodeId -> TQueue ForwardMessage -> Zeno r ()
runForwarder node@Node{..} nodeId chan = do
  withLocalResources do
    (_, eConn) <- allocate (liftIO mkConn) (either (\_ -> pure ()) NT.close)
    case eConn of
      Left e -> warnDidNotSend e
      Right conn -> do
        liftIO (NT.send conn [""]) >>=
          either warnDidNotSend (\() -> loopForward conn)
  
  where
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

  warnDidNotSend :: Show e => e -> Zeno r ()
  warnDidNotSend err = do
    logWarn $ "Forwarder: Error connecting to %s: %s" % (show nodeId, show err)


subscribe :: (Has Node r, Serialize i) => ProcessId -> Zeno r (RemoteReceiver i)
subscribe procId = do
  node@Node{..} <- asks has
  (_, recv) <- 
    allocate
      (atomically $ getReceiverSTM node procId)
      (\_ -> atomically $ STM.delete procId topics)
  pure recv

getReceiverSTM :: Serialize i => Node -> ProcessId -> STM (RemoteReceiver i)
getReceiverSTM Node{..} pid = do
  STM.lookup pid topics >>=
    \case
      Just r -> throwSTM $ TopicIsRegistered pid
      Nothing -> do
        recv <- newTQueue
        populateFromRecvCache recv
        let wrapped = WrappedReceiver $ wrappedReceive recv
        STM.insert wrapped pid topics
        pure recv
  where
  wrappedReceive chan nodeId bs = do
      case decode bs of
        Right a -> writeTQueue chan $ RemoteMessage nodeId a
        _ -> pure ()  -- Could have a "bad queue", ie redirect all decode failures
                      -- to a thread which monitors for peer mischief

  populateFromRecvCache recv = do
    (misses, nextCache) <- receiveCacheTake pid <$> readTVar recvCache
    writeTVar recvCache nextCache
    forM_ misses
      \(_, nodeId, bs) -> wrappedReceive recv nodeId bs

withRemoteMessage :: (NodeId -> a -> m b) -> RemoteMessage a -> m b
withRemoteMessage act (RemoteMessage nodeId msg) = act nodeId msg
