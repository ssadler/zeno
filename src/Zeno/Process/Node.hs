{-# LANGUAGE QuasiQuotes #-}

module Zeno.Process.Node where

import Data.Serialize
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified StmContainers.Map as STM

import Network.Simple.TCP
import Network.Socket (SockAddr(..))

import UnliftIO

import Zeno.Prelude hiding ((%), finally)
import Zeno.Process.Types
import Zeno.Process.Node.ReceiveMissCache
import Zeno.Process.Remote
import Zeno.Process.Spawn
import Zeno.Config


withNode :: NetworkConfig -> (Node -> Zeno () a) -> Zeno () a
withNode (NC host ourPort) act = do
  node <-
    liftIO do
      mforwarders <- STM.newIO
      topics <- STM.newIO
      recvCache <- newTVarIO mempty
      pure Node{..}

  let
    -- TODO: if using rio instead of runZeno here, we don't see the message from logDiedSync?
    -- Is it a logging thing?
    acceptConn rio s = runZeno PlainLog () $ logDiedSync (show s) (runConnection node s)

  withRunInIO \rio -> do
    listen host (show ourPort) $
      \(server, serverAddr) -> do
        rio do
          spawn "socket server" \_ -> do
            forever do
              void $ acceptFork server $ acceptConn rio
          logInfo $ [pf|Listening on %?|] serverAddr
          act node


  where
    x :: String
    x = "Synchronous exception accepting connection: "


data NetworkError
  = NetworkMischief String
  | NetworkMurphy String
  | NetworkUnsupported String
  | UnsupportedForeignHost SockAddr
  deriving (Show)

instance Exception NetworkError

data ConnectionClosed = ConnectionClosed deriving (Show)
instance Exception ConnectionClosed



runConnection :: Node -> (Socket, SockAddr) -> Zeno () ()
runConnection node (conn, sockAddr) = do
  -- TODO: A check that this node isn't spamming us with connections
  -- TODO: logDebug new connections
  handle (\ConnectionClosed -> mempty) do

    nodeId <-
      case sockAddr of
        SockAddrInet _ _ -> do
          let theirHost = takeWhile (/=':') (show sockAddr)
          header <- receiveLen 3
          case runGet ((,) <$> getWord8 <*> get) header of
            Right (0, port) -> do
              pure $ NodeId theirHost port
            Right (p, _) -> do
              throwIO $ NetworkUnsupported $ [pf|Unsupported protocol (%i) from %?|] p sockAddr
            Left s -> do
              murphy s
        other -> throwIO $ UnsupportedForeignHost other

    forever do
      recvWord32 >>= receiveMessage . fromIntegral >>= handleMessage node nodeId

  where
  murphy :: String -> Zeno () a
  murphy s = throwIO $ NetworkMurphy $ desc sockAddr s
    where desc = [pf|Invariant violation error somehow triggered by: %?: %s|]

  recvWord16 :: Zeno () Word16
  recvWord16 = (runGet get <$> receiveLen 2) >>= either murphy pure

  recvWord32 :: Zeno () Word32
  recvWord32 = (runGet get <$> receiveLen 4) >>= either murphy pure

  receiveMessage len = do
    when (len > 10000) do
      throwIO $ NetworkMischief $ [pf|%? sent oversize message: %?|] sockAddr len
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
        LT -> throwIO $ NetworkMurphy "More data received than expected"


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
               modifyTVar recvCache $ receiveCachePut miss
             Just (WrappedReceiver write) -> do
               write nodeId rem
