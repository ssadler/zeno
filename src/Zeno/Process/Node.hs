
module Zeno.Process.Node where

import Data.Binary
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Transport as NT
import qualified StmContainers.Map as STM
import UnliftIO

import Zeno.Prelude hiding (finally)
import Zeno.Process.Types
import Zeno.Process.Remote


startNode :: NT.Transport -> IO Node
startNode transport = do
  endpoint <- NT.newEndPoint transport <&> either (error . show) id
  mforwarders <- STM.newIO
  topics <- STM.newIO
  let node = Node{..}
  forkIO $ networkEventHandler node
  pure node


stopNode :: Node -> IO ()
stopNode Node{..} = do
  NT.closeEndPoint endpoint
  NT.closeTransport transport


-- | Network Event Handler

networkEventHandler :: Node -> IO ()
networkEventHandler node@Node{..} = do

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
                  atomically (STM.lookup to topics) >>=
                    \case
                      Nothing -> pure ()
                      Just (WrappedReceiver write) -> do
                        write nodeId rem
                  go conns

        NT.ErrorEvent (NT.TransportError err description) -> do
          case err of
            NT.EventEndPointFailed -> error "Unrecoverable: Local endpoint failed"   -- TODO: panic
            NT.EventTransportFailed -> error "Unrecoverable: Local transport failed" -- TODO: panic
            NT.EventConnectionLost addr -> do
              let nodeId = NodeId addr
              atomically $ quitRemoteForwader node nodeId
              go $ Map.filter (== nodeId) conns

        -- TODO: Maybe we don't need to run killProcess on
        -- ConnectionClosed and EventConnectionLost. One should imply the other, which comes first?
        NT.ConnectionClosed connId -> do
          case Map.lookup connId conns of
            Nothing -> pure ()
            Just nodeId -> do
              atomically $ quitRemoteForwader node nodeId
              go $ Map.delete connId conns

        NT.ReceivedMulticast _ _ -> do
          traceM "Received Multicast??"
          go conns              -- TODO: log?
        NT.EndPointClosed -> do
          traceM "EndpointClosed"
          pure ()               -- TODO: log


