{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

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

import Debug.Trace


type RemotePacket = RemoteMessage BSL.ByteString
type RemoteProcess i m p = MonadProcess RemotePacket m p
type RemoteProcessData = ProcessData RemotePacket

data RemoteMessage i = RemoteMessage
  { rNodeId :: NodeId
  , rMsg :: i
  } deriving (Typeable)

instance Functor RemoteMessage where
  fmap f (RemoteMessage nodeId a) = RemoteMessage nodeId (f a)

data ForwardMessage = ForwardMessage ProcessId BSL.ByteString
  deriving (Typeable)

-- | Each peer connection has a local forwarder process.
--   The forwarder process has a salted key so it can't be
--   guessed by other nodes. When it's attached connection goes down,
--   it gets killed. Other processes can listen for it's demise to 
--   perform cleanups.
getForwarder :: Node -> NodeId -> IO (ProcessHandle ForwardMessage)
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
          Right () -> runProcessT (loopForward conn) procData
      
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


sendRemote :: (Binary o, MonadProcess i m p) => NodeId -> ProcessId -> o -> p i m ()
sendRemote nodeId theirPid msg = do
  node <- procAsks myNode
  proc <- liftIO $ getForwarder node nodeId
  atomically $
    sendProcSTM proc $ ForwardMessage theirPid $ encode msg


monitorRemote :: MonadProcess i m p => NodeId -> p i m () -> p i m ()
monitorRemote nodeId onTerminate = do
  node@Node{..} <- procAsks myNode
  -- TODO: atomic, mask, etc
  Proc{..} <- liftIO $ getForwarder node nodeId
  async do
    waitCatch procAsync >>= \e -> onTerminate
  pure ()


nodeMonitorRemote :: ProcessBase m => Node -> NodeId -> m () -> m ()
nodeMonitorRemote node remoteNodeId onTerminate = do
  -- TODO: atomic, mask, etc
  Proc{..} <- liftIO $ getForwarder node remoteNodeId
  waitCatch procAsync >>= \e -> onTerminate
  pure ()


receiveWaitRemote :: (Binary i, MonadProcess RemotePacket m p) => p RemotePacket m (RemoteMessage i)
receiveWaitRemote = do
  pd <- procAsks id
  atomically do
    receiveWaitSTM pd >>= decodeReceivedOrRetry

decodeReceivedOrRetry :: Binary i => RemoteMessage BSL.ByteString -> STM (RemoteMessage i)
decodeReceivedOrRetry (RemoteMessage nodeId bs) = do
  case decodeOrFail bs of
    Right ("", _, a) -> pure $ RemoteMessage nodeId a
    _ -> do
      traceM "receiveWaitRemote miss"
      retrySTM -- TODO: log


receiveMaybeRemote :: (Binary i, MonadProcess (RemoteMessage BSL.ByteString) m p)
                   => p RemotePacket m (Maybe (RemoteMessage i))
receiveMaybeRemote = do
  pd <- procAsks id
  atomically do
    receiveMaybeSTM pd >>= maybe (pure Nothing) (fmap Just . decodeReceivedOrRetry)


-- | Network Event Handler

networkHandler :: ProcessT (ProcessData ()) IO ()
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
          traceM "Received Multicast??"
          go conns              -- TODO: log?
        NT.EndPointClosed -> do
          traceM "EndpointClosed"
          pure ()               -- TODO: log



-- TODO: This should really go away. It confuses the API.

withRemoteMessage :: (MonadProcess RemotePacket m p, Binary i)
                  => (NodeId -> i -> p RemotePacket m ()) -> RemoteMessage BSL.ByteString -> p RemotePacket m ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  case decodeOrFail bs of
    Right ("", _, a) -> act nodeId a
    Right (_, _, _) -> pure () -- TODO: log
    Left _ -> pure () -- TODO: log


fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
