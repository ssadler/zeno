
module Zeno.Process.Node where

import Data.Serialize
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Transport as NT
import qualified StmContainers.Map as STM
import UnliftIO

import Zeno.Prelude hiding (finally)
import Zeno.Process.Types
import Zeno.Process.Node.ReceiveMissCache
import Zeno.Process.Remote


startNode :: NT.Transport -> IO Node
startNode transport = do
  node <-
    liftIO do
      endpoint <- NT.newEndPoint transport <&> either (error . show) id
      mforwarders <- STM.newIO
      topics <- STM.newIO
      recvCache <- newTVarIO mempty
      pure Node{..}
  forkIO $ runZeno PlainLog () $ networkEventHandler node
  pure node


stopNode :: Node -> IO ()
stopNode Node{..} = do
  NT.closeEndPoint endpoint
  NT.closeTransport transport


-- | Network Event Handler

networkEventHandler :: Node -> Zeno () ()
networkEventHandler node@Node{..} = do

  fix1 mempty $
    \go !conns -> do
      evt <- liftIO $ NT.receive endpoint

      case evt of
        NT.ConnectionOpened connId _ theirEndpoint -> do
          go $ Map.insert connId (NodeId theirEndpoint) conns

        -- Ignore test sends
        NT.Received _connId [] -> go conns
        NT.Received _connId [""] -> go conns

        NT.Received connId bss -> do
          case Map.lookup connId conns of
            Nothing -> do
              logWarn "Got a message from unknown connection"
            Just nodeId -> do
              handleMessage nodeId bss
          go conns

        -- TODO: Maybe we don't need to run killProcess on
        -- ConnectionClosed and EventConnectionLost. One should imply the other, which comes first?
        NT.ConnectionClosed connId -> do
          case Map.lookup connId conns of
            Nothing -> pure ()
            Just nodeId -> do
              atomically $ quitRemoteForwader node nodeId
              go $ Map.delete connId conns

        NT.ErrorEvent (NT.TransportError err description) -> do
          case err of
            NT.EventEndPointFailed -> error "Unrecoverable: Local endpoint failed"   -- TODO: panic
            NT.EventTransportFailed -> error "Unrecoverable: Local transport failed" -- TODO: panic
            NT.EventConnectionLost addr -> do
              let nodeId = NodeId addr
              atomically $ quitRemoteForwader node nodeId
              go $ Map.filter (/= nodeId) conns

        NT.ReceivedMulticast _ _ -> do
          logWarn "Received Multicast?? Should not happen"
          go conns
        NT.EndPointClosed -> do
          logInfo "EndpointClosed"
          pure ()

  where

  handleMessage nodeId bss = do
    let bs = toS $ BSL.fromChunks bss
        (toB, rem) = BS.splitAt 16 bs
    if BS.length toB /= 16
       then logDebug $ "Could not decode packet from: %s" % show nodeId
       else do
         let to = ProcessId $ unsafeToFixed toB
         atomically do
           STM.lookup to topics >>=
             \case
               Nothing -> do
                 let miss = (to, nodeId, rem)
                 modifyTVar recvCache $ receiveCachePut miss
               Just (WrappedReceiver write) -> do
                 write nodeId rem
