{-# LANGUAGE OverloadedStrings, RecordWildCards, MonoLocalBinds, DeriveGeneric #-}

module Zeno.Consensus.P2P
  ( NewPeer(..)
  , P2P(..)
  , startP2P
  , makeNodeId
  , getPeers
  , sendPeers
  , runSeed
  ) where

import Network.Transport (EndPointAddress(..), Transport)
import Network.Socket (HostName, ServiceName)
import Network.Transport.TCP
import Network.Distributed

import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Data.Binary
import Data.Typeable

import Zeno.Prelude

import GHC.Generics (Generic)
import System.Posix.Signals
import UnliftIO
import UnliftIO.Concurrent


getPeers :: Process [NodeId]
getPeers = error "getPeers"


-- * Peer-to-peer API



type Peers = S.Set NodeId

data PeerState = PeerState { p2pPeers :: TVar Peers }

data P2P = P2P
  { p2pNode :: Node
  , p2pState :: PeerState
  }


data WhereIsReply = WhereIsReply String (Maybe ProcessId)
  deriving (Generic, Typeable)

instance Binary WhereIsReply


runSeed :: String -> Word16 -> IO ()
runSeed host port = do
  putStrLn $ "Now seeding on %s:%i" % (host, port)
  startP2P host port []
  threadDelay $ 2^64


-- ** Initialization

-- | Make a NodeId from "host:port" string.
makeNodeId :: Word16 -> String -> NodeId
makeNodeId port addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr', ":0"]
  where addr' = addr ++ maybe (':' : show port) (\_ -> "") (elemIndex ':' addr)



peerControllerPid :: ProcessId
peerControllerPid = serviceId "peerController"



startP2P
  :: HostName
  -> Word16
  -> [NodeId]
  -> IO P2P
startP2P host port seeds = do
  t <- getTransport
  node <- startNode t
  peerState <- PeerState <$> newTVarIO mempty
  nodeSpawnNamed node peerControllerPid $ peerController peerState seeds
  installHandler sigUSR1 (Catch $ dumpPeers peerState) Nothing
  pure $ P2P node peerState

  where
  myNodeId = makeNodeId port host

  (bindAddr, hostAddr) =
    case elemIndex '/' host of
      Nothing -> (host, host)
      Just idx -> let (a, _:b) = splitAt idx host in (a, b)

  tcpHost = Addressable $
    TCPAddrInfo bindAddr (show port) ((,) hostAddr)

  getTransport :: IO Transport
  getTransport = do
    let tcpParams = defaultTCPParameters { tcpCheckPeerHost = True }
    createTransport tcpHost tcpParams <&>
      either (error . show) id

  peerController :: PeerState -> [NodeId] -> Process ()
  peerController state seeds = do
    forever do
      mapM_ doDiscover seeds
      receiveDuringS 60 $
        withRemoteMessage $
          \peer ->
             \case 
               Hello     -> onPeerHello state peer
               (Peers peers) -> do
                 known <- readTVarIO $ p2pPeers state
                 -- Do a discovery here so when we get a response we know the node is up
                 mapM_ doDiscover $ S.toList $ S.difference peers known



data PeerMsg =
    Hello
  | Peers Peers
  deriving (Generic)

instance Binary PeerMsg

newtype GetPeers = GetPeers ProcessId
  deriving (Typeable)

dumpPeers :: PeerState -> IO ()
dumpPeers PeerState{..} = do
  peers <- atomically $ readTVar p2pPeers

  runZeno () $ do
    logInfo "Got signal USR1"
    forM_ peers $ \p ->
      logInfo $ show p



-- 0: A node probes another node
doDiscover :: NodeId -> Process ()
doDiscover nodeId =
  sendRemote nodeId peerControllerPid Hello


-- 2: When there's a request to share peers
onPeerHello :: PeerState -> NodeId -> Process ()
onPeerHello s@PeerState{..} peer = do
  peers <- readTVarIO p2pPeers
  sendRemote peer peerControllerPid peers
  newPeer s peer


newPeer :: PeerState -> NodeId -> Process ()
newPeer state@(PeerState{..}) peer = do
  peers <- readTVarIO p2pPeers
  unless (S.member peer peers) do
    say $ "New peer: " ++ show peer
    atomically $ writeTVar p2pPeers $ S.insert peer peers
    monitorRemote peer dropPeer
    -- nsend peerListenerService $ NewPeer $ processNodeId pid -- TODO
    sendRemote peer peerControllerPid Hello

  where
  dropPeer = do
    say $ "Peer disconnect: " ++ show peer
    atomically $ modifyTVar p2pPeers $ S.delete peer
 
say = undefined
 

-- 
-- | Broadcast a message to a specific service on all peers.
sendPeers :: Binary a => ProcessId -> a -> Process ()
sendPeers pid msg = do
  peers <- getPeers
  forM_ peers $ \peer -> sendRemote peer pid msg

-- 
-- 
-- 
-- 
-- --
-- 
newtype NewPeer = NewPeer NodeId
  deriving (Binary)
