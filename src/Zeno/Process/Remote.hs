
module Zeno.Process.Remote where

import Control.Concurrent.STM (stateTVar, throwSTM)
import Control.DeepSeq
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.FixedBytes
import Data.Hashable
import Data.Serialize
import Data.Typeable
import Data.Void
import GHC.Generics (Generic)

import Network.Simple.TCP

import qualified StmContainers.Map as STM
import UnliftIO hiding (Chan)

import UnliftIO.Concurrent

import Zeno.Process.Spawn hiding (send)
import Zeno.Process.Types
import Zeno.Prelude hiding (finally)


sendRemote :: (Serialize a, Has Node r) => NodeId -> CapabilityId -> a -> Zeno r ()
sendRemote nodeId capid msg = do
  let payload = encodeLazy (capid, msg)
  traceM "send remote"
  BSL.length payload `seq`                       -- `seq` avoids holding thunk longer than neccesary
    withRemoteForwarder nodeId \(chan, _) -> do
      writeTQueue chan payload


monitorRemote :: Has Node r => NodeId -> Zeno r () -> Zeno r ()
monitorRemote nodeId act = do
  ioAct <- toIO act
  withRemoteForwarder nodeId $ \(_, onQuit) -> do
    modifyTVar onQuit (>> ioAct)


-- TODO: Dejafu test
withRemoteForwarder :: Has Node r => NodeId -> (Forwarder -> STM a) -> Zeno r a
withRemoteForwarder nodeId act = do
  node <- asks has
  mask_ do
    ((chan, onQuit), created, r) <- atomically $ getCreateChan node
    when created do
      void $ forkIOWithUnmask \unmask -> do
        finally
          do unmask (runForwarder nodeId chan)
          do cleanup node chan onQuit
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
              timeoutSTMS 5 (readTQueue chan) >>=
                sendSizePrefixed conn . maybe "" id

  sendSizePrefixed conn bs = do
    let prefix = encodeLazy (fromIntegral (BSL.length bs) :: Word32)
    sendLazy conn $ prefix <> bs

  logTerminated :: SomeException -> Zeno r ()
  logTerminated e = do
    logInfo $ "Forwarder thread died with: %s" % show e


registerCapability :: Has Node r => Word8 -> (RemoteMessage BS.ByteString -> IO ()) -> Zeno r ()
registerCapability capid handler = do
  node@Node{..} <- asks has
  void $
    allocate
      do atomically do
          STM.lookup capid capabilities >>=
             \case
               Nothing -> STM.insert handler capid capabilities
               Just _ -> error $ "Capability registered: %s" % show capid
      (\_ -> atomically $ STM.delete capid capabilities)


withRemoteMessage :: (Monad m, Serialize a) => (NodeId -> a -> m ()) -> RemoteMessage BS.ByteString -> m ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  either (\_ -> traceM "could not decode" >> pure ()) (act nodeId) $ decode bs
