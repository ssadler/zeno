{-# LANGUAGE QuasiQuotes #-}

module Zeno.Process.Node where

import Data.Serialize
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified StmContainers.Map as STM

import Network.Simple.TCP
import Network.Socket (SockAddr(..), HostAddress, hostAddressToTuple, getSocketName)

import UnliftIO
import UnliftIO.Concurrent

import Zeno.Prelude hiding (finally)
import Zeno.Process.Types
import Zeno.Process.Node.ReceiveMissCache
import Zeno.Process.Remote
import Zeno.Process.Spawn


withNode :: NetworkConfig -> Zeno Node a -> Zeno () a
withNode (NetworkConfig host port) act = do
  withRunInIO \rio -> do
    listen host (show port) $ \(server, serverAddr) -> do
      node <- mkNode server
      rio do
        withLocalResources do -- This is neccesary so that the server thread gets killed
                              -- before the socket file descriptor is cleaned up. We also
                              -- want to bind the socket in the calling thread in case of any
                              -- error and this is the simplest way to do it.
          spawn "socket server" \_ -> do
            forever do
              acceptForkAsync server $ wrapRunConn node
          logInfo $ [pf|Listening on %?|] serverAddr
          withContext (const node) act
  where
  mkNode serverSock = do
    myNodeId <- fromString . show <$> getSocketName serverSock -- flimsy?
    mforwarders <- STM.newIO
    mreceivers <- STM.newIO
    topics <- STM.newIO
    missCache <- newTVarIO mempty
    pure Node{..}

  acceptForkAsync :: MonadUnliftIO m => Socket -> ((Socket, SockAddr) -> Async () -> m ()) -> m ()
  acceptForkAsync server act = do
    withRunInIO \rio -> do
      accept server \client -> do
        handoff <- newEmptyTMVarIO
        atomically . putTMVar handoff =<<
          async do
            atomically (takeTMVar handoff) >>= rio . act client

  wrapRunConn node s@(sock, sockAddr) asnc = do
    -- TODO: logDebug new connections
    -- Is it logging connection errors here?
    logDiedSync (show sockAddr) do
      limitInboundConnections node sockAddr asnc $
        runConnection node sock

limitInboundConnections :: Node -> SockAddr -> Async () -> (HostAddress -> Zeno () a) -> Zeno () a
limitInboundConnections Node{..} sockAddr asnc act = do
  case sockAddr of
    SockAddrInet _ ip@16777343 -> do
      -- 127.0.0.1 is special. TODO: Nice way not have to do this, and also be able
      -- to test locally?
      -- Ah, it could map port ranges to local IPs,
      -- eg any port between 10k and 11k is .1, etc, and replace the incoming IP.
      -- Except we don't have the server listen port here, for good reasons.
      act ip

    SockAddrInet _ ip -> do
      let
        run = do
          mapM_ cancel =<< atomically do
            STM.lookup ip mreceivers
              <* STM.insert asnc ip mreceivers
          act ip

      finally run do
          join do
            atomically do
              STM.lookup ip mreceivers >>= \case
                Nothing -> pure $ logMurphy "no value in mreceivers for ip"
                Just oasnc -> do
                  when (asnc == oasnc) (STM.delete ip mreceivers)
                  pure mempty

    other -> throwIO $ UnsupportedForeignHost other


data NetworkError
  = NetworkMischief String
  | NetworkMurphy String
  | NetworkUnsupported String
  | UnsupportedForeignHost SockAddr
  deriving (Show)

instance Exception NetworkError

data ConnectionClosed = ConnectionClosed deriving (Show)
instance Exception ConnectionClosed


runConnection :: Node -> Socket -> HostAddress -> Zeno () ()
runConnection node@Node{..} conn ip = do
  handle (\ConnectionClosed -> mempty) do -- Don't spam up the log
    nodeId <- readHeader
    forever do
      len <- (decode <$> receiveLen 4) >>= either murphy pure :: Zeno () Word32
      receiveMessage (fromIntegral len) >>= handleMessage node nodeId

  where
  murphy :: String -> Zeno () a
  murphy s = throwIO $ NetworkMurphy $ desc ip s
    where desc = [pf|Invariant violation error somehow triggered by: %?: %s|]

  receiveMessage len = do
    when (len > 10000) do
      throwIO $ NetworkMischief $ [pf|%? sent oversize message: %?|] (renderIp ip) len
    receiveLen len

  receiveLen :: Int -> Zeno () ByteString
  receiveLen 0 = pure ""
  receiveLen len = do
    fix2 "" len \f xs rem -> do
      s <- recv conn rem >>= maybe (throwIO ConnectionClosed) pure
      let newbs = xs <> BSL.fromStrict s
          newrem = rem - BS.length s
      case compare newrem 0 of
        EQ -> pure $ BSL.toStrict newbs
        GT -> f newbs newrem
        LT -> murphy "More data received than expected"

  readHeader = do
    header <- receiveLen 3
    case decode header of
      Right ('\0', port) -> pure $ NodeId (renderIp ip) port
      Right (p, _) -> do
        throwIO $ NetworkUnsupported $ [pf|Unsupported protocol (%?) from %?|] p ip
      Left s -> do
        murphy s

renderIp :: HostAddress -> String
renderIp ip = "%i.%i.%i.%i" % hostAddressToTuple ip

handleMessage :: Node -> NodeId -> BS.ByteString -> Zeno () ()
handleMessage _ _ "" = mempty
handleMessage Node{..} nodeId bs = do
  let (toB, rem) = BS.splitAt 16 bs
  if BS.length toB /= 16
     then logDebug $ [pf|Could not decode packet from: %?|] nodeId
     else do
       let to = ProcessId $ unsafeToFixed toB
       atomically do
         STM.lookup to topics >>=
           \case
             Nothing -> do
               let miss = (to, nodeId, rem)
               modifyTVar missCache $ receiveCachePut miss
             Just (WrappedReceiver write) -> do
               write nodeId rem
