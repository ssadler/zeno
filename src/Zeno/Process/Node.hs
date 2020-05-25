
module Zeno.Process.Node where

import Data.Binary
import qualified Data.IntMap.Strict as IntMap
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
  node <-
    liftIO do
      endpoint <- NT.newEndPoint transport <&> either (error . show) id
      mforwarders <- STM.newIO
      topics <- STM.newIO
      recvCache <- newTVarIO mempty
      pure Node{..}
  forkIO $ runZeno () $ networkEventHandler node
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
        NT.Received connId [] -> go conns
        NT.Received connId [""] -> go conns

        NT.Received connId bss -> do
          case Map.lookup connId conns of
            Nothing -> do
              logWarn "Got a message from unknown connection" >> go conns
            Just nodeId -> do
              let bs = BSL.fromChunks bss
              case decodeOrFail bs of
                Left (_, _, errStr) -> do
                  logDebug $ "Could not decode packet from: %s" % show nodeId
                  go conns
                Right (rem, _, to) -> do
                  atomically do
                    STM.lookup to topics >>=
                      \case
                        Nothing -> do
                          cacheTopicMiss to nodeId rem
                        Just (WrappedReceiver write) -> do
                          write nodeId rem
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
              go $ Map.filter (== nodeId) conns

        NT.ReceivedMulticast _ _ -> do
          logWarn "Received Multicast?? Should not happen"
          go conns
        NT.EndPointClosed -> do
          logInfo "EndpointClosed"
          pure ()


  where
  cacheMaxSize = 1000

  cacheTopicMiss :: ProcessId -> NodeId -> BSL.ByteString -> STM ()
  cacheTopicMiss processId nodeId msg = do
    let
      item = (processId, nodeId, msg)
      next cache
        | length cache == 0 = IntMap.singleton 0 item
        | length cache == cacheMaxSize =
            let ((minId, _), nextCache) = IntMap.deleteFindMin cache
             in IntMap.insert (minId + length cache) item nextCache
        | otherwise =
            let (minId, _) = IntMap.findMin cache
             in IntMap.insert (minId + length cache) item cache
 
    cache <- readTVar recvCache
    writeTVar recvCache $ next cache
