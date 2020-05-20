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

data ForwardMessage = ForwardMessage ProcessId BSL.ByteString
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
  run procData = do
    mConn <- NT.connect endpoint
                        (endpointAddress nodeId)
                        NT.ReliableOrdered
                        NT.defaultConnectHints
    case mConn of
      Right conn -> do
        didSend <- NT.send conn (BSL.toChunks $ encode ())
        case didSend of
          Left _   -> do
            putStrLn "Forwarded: Error setting up lightweight connection, did not send"
          Right () -> runReaderT (loopForward conn) procData
      
      Left err -> do
        putStrLn $ "Forwarded: Error setting up lightweight connection: " ++ show err

  loopForward conn = do
    receiveWait >>=
      \(ForwardMessage procId msg) -> do
        let packet = BSL.toChunks $ encode procId <> msg
        _ <- liftIO $ NT.send conn packet -- TODO: failure? Die on failure
        loopForward conn


getForwarderId :: BS.ByteString -> NodeId -> ProcessId
getForwarderId salt nodeId = 
  ProcessId $ toFixed $ blake2b_160 $ salt <> BSL.toStrict (encode nodeId)


sendRemote :: (Binary a, Process p) => NodeId -> ProcessId -> a -> p ()
sendRemote nodeId theirPid msg = do
  node <- procAsks myNode
  proc <- liftIO $ getForwarder node nodeId
  atomically $
    sendProcSTM proc $ ForwardMessage theirPid $ encode msg


monitorRemote :: Process p => NodeId -> p () -> p ()
monitorRemote nodeId onTerminate = do
  node@Node{..} <- procAsks myNode
  -- TODO: atomic, mask, etc
  Proc{..} <- liftIO $ getForwarder node nodeId
  spawn do
    waitCatch procAsync >>= \e -> onTerminate
  pure ()


nodeMonitorRemote :: ProcessBase m => Node -> NodeId -> m () -> m ()
nodeMonitorRemote node remoteNodeId onTerminate = do
  -- TODO: atomic, mask, etc
  Proc{..} <- liftIO $ getForwarder node remoteNodeId
  waitCatch procAsync >>= \e -> onTerminate
  pure ()


receiveWaitRemote :: (Binary a, Process p) => p (NodeId, a)
receiveWaitRemote = do
  RemoteMessage nodeId bs <- receiveWait
  case decodeOrFail bs of
    Right ("", _, a) -> pure (nodeId, a)
    Right (_, _, _) -> receiveWaitRemote -- TODO: log
    Left _ -> receiveWaitRemote -- TODO: log
  


-- | Network Event Handler

networkHandler :: ReaderT ProcessData IO ()
networkHandler = do
  node@Node{..} <- procAsks myNode

  fix1 mempty $
    \go !conns -> do
      evt <- liftIO $ NT.receive endpoint

      case evt of
        NT.ConnectionOpened connId _ theirEndpoint -> do
          go $ Map.insert connId (NodeId theirEndpoint) conns

        -- Test packet from peer
        NT.Received connId [] -> go conns

        NT.Received connId bss -> do
          case Map.lookup connId conns of
            Nothing -> do
              go conns                                        --  TODO: log the bad thing?
            Just nodeId -> do
              let bs = BSL.fromChunks bss
              case decodeOrFail bs of
                Left (_, _, errStr) -> do
                  go conns               -- TODO: log?
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

        NT.ReceivedMulticast _ _ -> do
          go conns              -- TODO: log?
        NT.EndPointClosed -> do
          pure ()               -- TODO: log



-- TODO: This should really go away. It confuses the API.

withRemoteMessage :: (Process p, Binary i) => (NodeId -> i -> p ()) -> RemoteMessage -> p ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  case decodeOrFail bs of
    Right ("", _, a) -> act nodeId a
    Right (_, _, _) -> pure () -- TODO: log
    Left _ -> pure () -- TODO: log

    



fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
