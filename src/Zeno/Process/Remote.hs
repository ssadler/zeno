{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}

module Zeno.Process.Remote where

import Control.Concurrent.STM (stateTVar, throwSTM)
import Control.DeepSeq
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.FixedBytes
import Data.Hashable
import qualified Data.Map as Map
import Data.Serialize
import Data.Tuple (swap)
import Data.Typeable
import Data.Void
import GHC.Generics (Generic)

import Network.Simple.TCP

import UnliftIO hiding (Chan)

import UnliftIO.Concurrent

import Zeno.Process.Spawn hiding (send)
import Zeno.Process.Types
import Zeno.Prelude hiding (finally)


sendRemote :: (Serialize a, HasNode m) => NodeId -> CapabilityId -> a -> m ()
sendRemote nodeId capid obj = do
  let payload = encodeLazy obj
  sendRemoteBS nodeId capid payload

instance Has Node r => HasNode (Zeno r) where
  type HandlerMonad (Zeno r) = IO

  sendRemoteBS nodeId capid bs = do
    withRemoteForwarder nodeId \(chan, _) -> do
      atomically $ writeTQueue chan $ encodeLazy capid <> bs

  monitorRemote nodeId act = do
    ioAct <- toIO act
    withRemoteForwarder nodeId $ \(_, onQuit) -> do
      modifyIORef onQuit (>> ioAct)

  registerCapability capid handler = do
    node@Node{..} <- asks has
    void $
      modifyIORef capabilities \caps -> do
        case Map.lookup capid caps of
          Nothing -> Map.insert capid handler caps
          Just _ -> error $ "Capability registered: %s" % show capid

  getMyIp = asks $ hostName . myNodeId . has


withRemoteForwarder :: Has Node r => NodeId -> (Forwarder -> Zeno r a) -> Zeno r a
withRemoteForwarder nodeId act = do
  node@Node{..} <- asks has
  mask_ do
    join do
      modifyMVar mforwarders $ fmap swap . runStateT do
        use (at nodeId) >>=
          \case
            Just fwd -> pure $ act fwd
            Nothing -> do
              chan <- newTQueueIO
              onQuit <- newIORef mempty
              at nodeId .= Just (chan, onQuit)
              pure $ act (chan, onQuit) <*      -- The action should come first in order to
                                                -- register destruct handlers before it has 
                                                -- a chance to quit
                forkIOWithUnmask \unmask -> do
                  finally
                    do unmask $ runForwarder nodeId chan
                    do quitForwarder nodeId chan onQuit


runForwarder :: Has Node r => NodeId -> TQueue ForwardMessage -> Zeno r ()
runForwarder nodeId@NodeId{..} chan = do
  logDiedSync ("OUT(%s)" % show nodeId) do
    handleIO mempty do
      withLocalResources do
        withRunInIO \rio -> do
          connect hostName (show hostPort) \(conn, _) -> do
            rio do
              sendHeader conn
              forever do
                timeoutSTMS 10 (readTQueue chan) >>=
                  sendSizePrefixed conn . maybe "" id
  where
  sendHeader conn = do
    NodeId _ myPort <- asks $ myNodeId . has
    send conn $ encode (0 :: Word8, myPort)

  sendSizePrefixed conn bs = do
    let prefix = encodeLazy (fromIntegral (BSL.length bs) :: Word32)
    sendLazy conn $ prefix <> bs


quitForwarder :: Has Node r => NodeId -> TQueue ForwardMessage -> IORef (IO ()) -> Zeno r ()
quitForwarder nodeId chan onQuit = do
  Node{..} <- asks has
  modifyMVar mforwarders $ fmap swap . runStateT do
    use (at nodeId) >>=
      mapM_ \(chan', _) -> do
        if chan' == chan
           then do
             at nodeId .= Nothing
           else do
             -- Given that this is the only location that deletes a forwarder...
             logMurphy "How is there another chan with the same key?"

  join $ liftIO <$> readIORef onQuit



withRemoteMessage :: (Monad m, Serialize a) => (NodeId -> a -> m ()) -> RemoteMessage LazyByteString -> m ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  either (\_ -> traceM "could not decode" >> pure ()) (act nodeId) $ decodeLazy bs
