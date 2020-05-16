{-# LANGUAGE OverloadedStrings, RecordWildCards, MonoLocalBinds, DeriveGeneric #-}

-- Kindly borrowed from: http://hackage.haskell.org/package/distributed-process-p2p
--
--
-- | Peer-to-peer node discovery backend for Cloud Haskell based on the TCP
-- transport. Provided with a known node address it discovers and maintains
-- the knowledge of it's peers.
--
-- > import qualified Control.Distributed.Backend.P2P as P2P
-- > import           Control.Monad.Trans (liftIO)
-- > import           Control.Concurrent (threadDelay)
-- >
-- > main = P2P.bootstrap "myhostname" "9001" [P2P.makeNodeId "seedhost:9000"] $ do
-- >     liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
-- >     P2P.nsendPeers "myService" ("some", "message")

module Zeno.Consensus.P2P (
    -- * Starting peer controller
    NewPeer(..),
    startP2P,
    makeNodeId,
    getPeers,
    nsendPeers,
    runSeed
) where

import Network.Transport (EndPointAddress(..), Transport)
import Network.Socket (HostName, ServiceName)
<<<<<<< HEAD
import Network.Transport.TCP
=======
import Network.Transport.TCP (createTransport, defaultTCPParameters, defaultTCPAddr)
<<<<<<< HEAD
import Network.NQE
>>>>>>> type checking with undefined networking
=======
import Network.Distributed
>>>>>>> halfway through implementing the guts of the new network layer. Time to look at the receive side.

import Control.Monad

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S
import Data.Binary
import Data.Typeable

import Zeno.Prelude
import Zeno.Consensus.P2P.Types

import GHC.Generics (Generic)
import System.Posix.Signals
import UnliftIO
import UnliftIO.Concurrent

getPeers = undefined


-- * Peer-to-peer API



type Peers = S.Set RemoteProcessId

data PeerState = PeerState { p2pPeers :: TVar Peers }

data P2P = P2P
  { node :: Node
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


<<<<<<< HEAD
createLocalNode
  :: HostName
  -> ServiceName
  -> IO LocalNode
createLocalNode host port = do
  let tcpParams = defaultTCPParameters { tcpCheckPeerHost = True }
  transport <- either (error . show) id
             <$> createTransport tcpHost tcpParams
  newLocalNode transport
  where
  (bindAddr, hostAddr) =
    case elemIndex '/' host of
      Nothing -> (host, host)
      Just idx -> let (a, _:b) = splitAt idx host in (a, b)

  tcpHost = Addressable $
    TCPAddrInfo bindAddr port ((,) hostAddr)
=======
>>>>>>> halfway through implementing the guts of the new network layer. Time to look at the receive side.

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
  node <- createNode t
  peerState <- PeerState <$> newTVarIO mempty
  nodeSpawnNamed node peerControllerPid $ peerController peerState seeds
  installHandler sigUSR1 (Catch $ dumpPeers peerState) Nothing
  pure $ P2P node peerState

  where
  myNodeId = makeNodeId port host

  getTransport :: IO Transport
  getTransport = do
    let tcpHost = defaultTCPAddr host (show port)
    et <- createTransport tcpHost defaultTCPParameters
    either (fail . show) pure et

  peerController :: PeerState -> [NodeId] -> Process ()
  peerController state seeds = do
    self <- RPID (error "remoteself") <$> getSelfPid

    forever $ do
      mapM_ doDiscover seeds
      repeatMatch 60000000 [ match $ onPeerHello state
                           , match $ onPeerResponse state
                           --, match $ onMonitor state
                           ]


newtype GetPeers = GetPeers ProcessId
  deriving (Typeable)

dumpPeers :: PeerState -> IO ()
dumpPeers PeerState{..} = do
  peers <- atomically $ readTVar p2pPeers

  runZeno () $ do
    logInfo "Got signal USR1"
    forM_ peers $ \p ->
      logInfo $ show p


--onMonitor = undefined
onPeerResponse = undefined

-- ** Discovery

-- 0: A node probes another node
doDiscover :: NodeId -> Process ()
doDiscover nodeId = sendRemote rpid Hello
  where rpid = RPID nodeId peerControllerPid


-- 2: When there's a request to share peers
onPeerHello :: PeerState -> (RemoteProcessId, Hello) -> Process ()
onPeerHello s@PeerState{..} (peer, Hello) = do
  peers <- readTVarIO p2pPeers
  sendRemote peer peers
  newPeer s peer


-- 3: When peers are received
onPeerResponse :: PeerState -> (ProcessId, Peers) -> Process ()
-- onPeerResponse state (peer, peers) = do
--     known <- readMVar $ p2pPeers state
--     -- Do a discovery here so when we get a response we know the node is up
--     mapM_ (doDiscover . processNodeId) $ S.toList $ S.difference peers known
-- 
-- -- 4: Disconnect
--onMonitor :: PeerState -> ProcessMonitorNotification -> Process ()
-- onMonitor PeerState{..} (ProcessMonitorNotification mref pid reason) = do
--     say $ "Dropped peer: " ++ show (pid, reason)
--     maybe (return ()) unmonitor $ Just mref
--     modifyMVar_ p2pPeers $ pure . S.delete pid
 

newPeer :: PeerState -> RemoteProcessId -> Process ()
newPeer (PeerState{..}) pid = do
  pids <- readTVarIO p2pPeers
  unless (S.member pid pids) do
    say $ "New peer:" ++ show pid
    -- putMVar p2pPeers $ S.insert pid pids
    -- -- _ <- monitor pid
    -- nsend peerListenerService $ NewPeer $ processNodeId pid
    -- self <- getSelfPid
    -- send pid (self, Hello)
 
say = undefined
 
 
data Hello = Hello
  deriving (Typeable, Generic)

instance Binary Hello
-- 
-- | Broadcast a message to a specific service on all peers.
nsendPeers :: Sendable a => String -> a -> Process ()
nsendPeers service msg = undefined -- getPeers >>= mapM_ (\peer -> nsendRemote peer service msg)
-- 
-- 
-- 
-- 
-- --
-- 
newtype NewPeer = NewPeer NodeId
  deriving (Binary)
