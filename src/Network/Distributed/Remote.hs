{-# LANGUAGE BangPatterns #-}

module Network.Distributed.Remote where

import Data.Binary
import Data.FixedBytes
import Data.Typeable
import qualified Data.Map as Map
import Control.Monad.Reader

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Network.Distributed.Types
import Network.Distributed.Process
import qualified Network.Transport as NT

import UnliftIO


data RemoteMessage = RemoteMessage NodeId BSL.ByteString
  deriving (Typeable)


-- | Each peer connection has a local forwarder process.
--   The forwarder process has a salted key so it can't be
--   guessed by other nodes. When it's attached connection goes down,
--   it gets killed. Other processes can listen for it's demise to 
--   perform cleanups.
getForwarder :: Node -> NodeId -> IO ProcessHandle
getForwarder node@Node{..} nodeId = do
  let pid = getForwarderId salt nodeId

  -- TODO: this needs to be atomic. withAtomicIO :: Node -> ...
  mproc <- atomically $ getProcessById node pid
  maybe (nodeSpawnNamed node pid run) pure mproc

  where
  run = runReaderT do
    liftIO (setupConn endpoint nodeId) >>=
      \case Nothing   -> pure ()                -- TODO: log?
            Just conn -> loopHandle conn

  loopHandle conn = do
    receiveWait >>=
      \pm -> do
        _ <- liftIO $ NT.send conn pm -- TODO: failure? Die on failure
        loopHandle conn


getForwarderId :: BS.ByteString -> NodeId -> ProcessId
getForwarderId salt nodeId = 
  ProcessId $ toFixed $ blake2b_160 $ salt <> BSL.toStrict (encode nodeId)


setupConn :: NT.EndPoint -> NodeId -> IO (Maybe NT.Connection)
setupConn endpoint nodeId = do
  mConn <- NT.connect endpoint
                      (endpointAddress nodeId)
                      NT.ReliableOrdered
                      NT.defaultConnectHints
  case mConn of
    Left _ -> pure Nothing
    Right conn -> do
      didSend <- NT.send conn (BSL.toChunks $ encode ())
      case didSend of
        Left _   -> pure Nothing
        Right () -> pure $ Just conn




sendRemote :: (Binary a, Process p) => NodeId -> ProcessId -> a -> p ()
sendRemote nodeId theirPid msg = do
  node <- procAsks node
  proc <- liftIO $ getForwarder node nodeId
  atomically $
    sendProcSTM proc $ RemoteMessage nodeId $ encode (theirPid, msg)



monitorRemote :: Process p => NodeId -> p () -> p ()
monitorRemote nodeId onTerminate = do
  node@Node{..} <- procAsks node
  -- TODO: atomic, mask, etc
  Proc{..} <- liftIO $ getForwarder node nodeId
  spawn do
    waitCatch procAsync >>= \_ -> onTerminate
  pure ()




-- | Network Event Handler

networkHandler :: ReaderT ProcessData IO ()
networkHandler = do
  node@Node{..} <- procAsks node

  fix1 mempty $
    \go !conns -> do

      evt <- liftIO $ NT.receive endpoint
      case evt of
        NT.ConnectionOpened connId _ theirEndpoint -> do
          go $ Map.insert connId (NodeId theirEndpoint) conns

        NT.Received connId bss -> do
          case Map.lookup connId conns of
            Nothing -> do
              go conns                                        --  TODO: log very bad thing?
            Just nodeId -> do
              let bs = BSL.fromChunks bss
              case decodeOrFail (BSL.fromChunks bss) of
                Left (_, _, errStr) -> go conns               -- TODO: log?
                Right (rem, _, to) -> do
                  send to $ RemoteMessage nodeId rem
                  go conns

        NT.ErrorEvent (NT.TransportError err description) -> do
          case err of
            NT.EventEndPointFailed -> error "Unrecoverable: Local endpoint failed"   -- TODO: panic
            NT.EventTransportFailed -> error "Unrecoverable: Local transport failed" -- TODO: panic
            NT.EventConnectionLost addr -> do
              let nodeId = NodeId addr
              killProcess node $ getForwarderId salt nodeId
              go $ Map.filter (== nodeId) conns

        -- TODO: Maybe we don't need to run killProcess on
        -- ConnectionClosed and EventConnectionLost. One should imply the other, which comes first?
        NT.ConnectionClosed connId -> do
          case Map.lookup connId conns of
            Nothing -> pure ()
            Just nodeId -> do
              killProcess node $ getForwarderId salt nodeId
              go $ Map.delete connId conns

        NT.ReceivedMulticast _ _ -> go conns              -- TODO: log?
        NT.EndPointClosed -> pure ()                      -- TODO: log



withRemoteMessage :: (Process p, Binary i) => (NodeId -> i -> p ()) -> RemoteMessage -> p ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  case decodeOrFail bs of
    Right ("", _, a) -> act nodeId a
    Right (_, _, _) -> pure () -- TODO: log
    Left _ -> pure () -- TODO: log

    



fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
