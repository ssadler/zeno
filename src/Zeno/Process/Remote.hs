
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

import Zeno.Process.Spawn hiding (send)
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

  cleanup :: Node -> TQueue ForwardMessage -> TVar (IO ()) -> Zeno r ()
  cleanup Node{..} chan onQuit = do
    join $
      atomically do
        STM.lookup nodeId mforwarders >>= \case
          Nothing -> pure mempty
          Just (chan', _) -> do
            if chan' == chan
               then do
                 STM.delete nodeId mforwarders
                 pure mempty
               else do
                 -- Given that this is the only location that deletes a forwarder...
                 pure $ logMurphy "How is there another chan with the same key?"

    join $ liftIO <$> readTVarIO onQuit


runForwarder :: Has Node r => NodeId -> TQueue ForwardMessage -> Zeno r ()
runForwarder nodeId@NodeId{..} chan = do
  logDiedSync ("outbound(%s)" % show nodeId) do
    handleIO mempty run
  where
  run = do
    withLocalResources do
      withRunInIO \rio -> do
        connect hostName (show hostPort) \(conn, _) -> do
          rio do
            NodeId _ myPort <- asks $ myNodeId . has
            let header = encode (0 :: Word8, myPort)
            send conn header
            forever do
              timeoutSTM 500000 (readTQueue chan) >>=
                sendSizePrefixed conn . maybe "" id

  sendSizePrefixed conn bs = do
    let prefix = encodeLazy (fromIntegral (BSL.length bs) :: Word32)
    sendLazy conn $ prefix <> bs

  logTerminated :: SomeException -> Zeno r ()
  logTerminated e = do
    logInfo $ "Forwarder thread died with: %s" % show e


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
    (misses, nextCache) <- receiveCacheTake pid <$> readTVar missCache
    writeTVar missCache nextCache
    forM_ misses
      \(_, nodeId, bs) -> wrappedReceive recv nodeId bs

withRemoteMessage :: (NodeId -> a -> m b) -> RemoteMessage a -> m b
withRemoteMessage act (RemoteMessage nodeId msg) = act nodeId msg
