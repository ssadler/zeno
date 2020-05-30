
module Zeno.Process.Node where

import Data.Serialize
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified StmContainers.Map as STM

import Network.Simple.TCP
import Network.Socket (SockAddr(..))

import UnliftIO

import Zeno.Prelude hiding (finally)
import Zeno.Process.Types
import Zeno.Process.Node.ReceiveMissCache
import Zeno.Process.Remote
import Zeno.Process.Spawn
import Zeno.Config


withNode :: NetworkConfig -> (Node -> Zeno () a) -> Zeno () a
withNode (NC host port) act = do
  node <-
    liftIO do
      mforwarders <- STM.newIO
      topics <- STM.newIO
      recvCache <- newTVarIO mempty
      pure Node{..}


  withRunInIO \rio -> do
    listen host (show port) $
      \(server, serverAddr) -> do
        rio do
          spawn "socket server" \_ -> do
            forever do
              -- catch
                (void $ acceptFork server (rio . runConnection node))
                -- (\e -> logWarn (x ++ show (e :: SomeException)))

          act node

  where
    x :: String
    x = "Synchronous exception accepting connection: "


data NetworkError
  = NetworkMischief String
  | NetworkMurphy String
  | NetworkUnsupported String
  | UnsupportedForeignHost SockAddr
  | ConnectonClosed
  deriving (Show)

instance Exception NetworkError




runConnection :: Node -> (Socket, SockAddr) -> Zeno () ()
runConnection node (conn, sockAddr) = do
  -- TODO: A check that this node isn't spamming us with connections
  -- TODO: logDebug new connections
  withException run logTerminated
  where
  run = do
    nodeId <-
      case sockAddr of
        SockAddrInet _ _ -> do
          let theirHost = takeWhile (/=':') (show sockAddr)
          header <- receiveLen 3
          case runGet ((,) <$> getWord8 <*> getWord16be) header of
            Right (0, port) -> do
              pure $ NodeId (theirHost, show port)
            Right (p, _) -> do
              throwIO $ NetworkUnsupported $ "Unsupported protocol (%i) from %s" % (p, show sockAddr)
            Left s -> do
              murphy s
        other -> throwIO $ UnsupportedForeignHost other

    forever do
      recvWord32 >>= receiveMessage . fromIntegral >>= handleMessage node nodeId

  murphy :: String -> Zeno () a
  murphy s = throwIO $ NetworkMurphy $ desc % (show sockAddr, s)
    where desc = "Invariant violation error somehow triggered by: %s: %s"

  recvScalar :: Serialize a => Int -> Zeno () a
  recvScalar len = do
    ea <- decode <$> receiveLen len
    either (murphy) pure ea

  recvWord16 :: Zeno () Word16
  recvWord16 = recvScalar 2

  recvWord32 :: Zeno () Word16
  recvWord32 = recvScalar 4

  receiveMessage len = do
    when (len > 10000) do
      throwIO $ NetworkMischief $ printf "%s sent oversize message: %s" % (show sockAddr, len)
    receiveLen len

  receiveLen len = do
    fix2 "" len \f xs rem -> do
      s <- recv conn rem >>= maybe (throwIO ConnectonClosed) pure
      let newbs = xs <> BSL.fromStrict s
          newrem = rem - BS.length s
      case compare newrem 0 of
        EQ -> pure $ BSL.toStrict newbs
        GT -> f newbs newrem
        LT -> throwIO $ NetworkMurphy "More data received than expected"

  logTerminated :: SomeException -> Zeno () ()
  logTerminated e = do
    logInfo $ "Receiver thread died with: %s" % show e



handleMessage :: Node -> NodeId -> BS.ByteString -> Zeno () ()
handleMessage Node{..} nodeId bs = do
  let (toB, rem) = BS.splitAt 16 bs
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
