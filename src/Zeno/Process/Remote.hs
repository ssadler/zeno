
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

import Network.Simple.TCP

import qualified StmContainers.Map as STM
import UnliftIO hiding (Chan)

import UnliftIO.Concurrent
import Unsafe.Coerce

import Zeno.Process.Types
import Zeno.Process.Node.ReceiveMissCache
import Zeno.Prelude hiding (finally)


sendRemote :: (Serialize a, Has Node r) => NodeId -> ProcessId -> a -> Zeno r ()
sendRemote nodeId pid msg = do
  withRemoteForwarder nodeId \(chan, _) -> do
    writeTQueue chan $ encodeLazy (pid, msg)


monitorRemote :: Has Node r => NodeId -> Zeno r () -> Zeno r ()
monitorRemote nodeId act = do
  ioAct <- toIO act
  withRemoteForwarder nodeId $ \(_, onQuit) -> do
    modifyTVar onQuit (>> ioAct)


withRemoteForwarder :: Has Node r => NodeId -> (Forwarder -> STM a) -> Zeno r a
withRemoteForwarder nodeId act = do
  node <- asks has
  mask_ do
    ((chan, onQuit), created, r) <- atomically $ getCreateChan node
    when created do
      void $ forkIOWithUnmask \unmask -> do
        -- TODO: logDebug with exceptions
        unmask (runForwarder nodeId chan) `finally` cleanup node chan onQuit
    pure r
  where
  getCreateChan Node{..} = do
    (fwd, created) <- do
      STM.lookup nodeId mforwarders >>=
        \case
          Just fwd -> pure (fwd, False)
          Nothing -> do
            fwd <- (,) <$> newTQueue <*> newTVar mempty
            STM.insert fwd nodeId mforwarders
            pure (fwd, True)

    (fwd, created,) <$> act fwd

  cleanup Node{..} chan onQuit = do
    join $
      atomically do
        STM.lookup nodeId mforwarders >>=
          mapM_ \(chan', onQuit) -> do
            when (chan' == chan) do
              STM.delete nodeId mforwarders

        liftIO <$> readTVar onQuit


runForwarder :: NodeId -> TQueue ForwardMessage -> Zeno r ()
runForwarder nodeId chan = do
  withLocalResources do
    withRunInIO \rio -> do
      connect hostName serviceName \(conn, _) -> do
        send conn "0"
        rio $ loopForward conn
  where
  NodeId (hostName, serviceName) = nodeId

  loopForward :: Socket -> Zeno r ()
  loopForward conn = do
    forever do
      atomically (readTQueue chan) >>= sendLazy conn


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
