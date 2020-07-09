{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Strict #-}

module Zeno.Consensus.P2P where

import Control.Monad.State
import qualified Data.Set as Set

import Network.Socket (HostName)

import UnliftIO

import Zeno.Console
import Zeno.Process
import Zeno.Prelude
import Zeno.Signal

-- Lenses have to go at the top ----------------------------------------------

data PeerState = PeerState
  { p2pPeers :: Set.Set NodeId
  , p2pDisconnects :: Set.Set HostName
  }

makeLensesUnderscored ''PeerState

-- P2P API --------------------------------------------------------------------

data P2PNode = P2PNode
  { p2pNode :: Node
  , p2pProc :: PeerProc
  }

instance Has Node     P2PNode where has = p2pNode
instance Has PeerProc P2PNode where has = p2pProc

type PeerProc = MVar PeerState

class HasNode m => HasP2P m where
  getPeers :: m [NodeId]

instance HasP2P (Zeno P2PNode) where
  getPeers = liftP2P $ Set.toList <$> use _peers


-- P2P Internal ---------------------------------------------------------------

p2pCapId :: CapabilityId
p2pCapId = 1

type P2P m = StateT PeerState m

emptyPeerState :: PeerState
emptyPeerState = PeerState mempty mempty

class (MonadLoggerUI m, HasNode m) => LiftP2P m where
  liftP2P :: P2P m a -> m a

instance LiftP2P (Zeno P2PNode) where
  liftP2P act = do
    asks has >>= flip queryP2P act

queryP2P :: MonadUnliftIO m => PeerProc -> P2P m a -> m a
queryP2P proc act = do
  modifyMVar proc $ fmap swap . runStateT act

startP2P :: [NodeId] -> Zeno Node PeerProc
startP2P seeds = do
  proc <- newMVar emptyPeerState
  withContext (\n -> P2PNode n proc) $ startPeerController seeds
  installSignalHandler sigUSR1 $ dumpPeers proc
  pure proc
  where
  dumpPeers proc = queryP2P proc do
    peers <- use _peers
    logInfo $ "Peers: " ++ show (length peers)
    disconnects <- Set.toList <$> use _disconnects
    logInfo $ "Disconnects: " ++ show disconnects

startPeerController :: [NodeId] -> Zeno P2PNode ()
startPeerController seeds = do
  myIp <- getMyIp
  rio <- askRunInIO

  registerCapability p2pCapId $
    withRemoteMessage \nodeId msg -> do
      when (hostName nodeId /= myIp) do
        rio $ liftP2P $
          onPeerMessage nodeId msg

  spawnNoHandle "peerController" do
    forever do
      forM_ seeds doDiscover
      liftP2P $ _disconnects .= mempty
      threadDelayS 60

data PeerMsg = GetPeers | Peers (Set.Set NodeId)
  deriving (Eq, Show, Generic)

instance Serialize PeerMsg

doDiscover :: HasNode m => NodeId -> m ()
doDiscover nodeId = do
  myIp <- getMyIp
  when (hostName nodeId /= myIp) do
    sendRemote nodeId p2pCapId GetPeers

onPeerMessage :: LiftP2P m => NodeId -> PeerMsg -> P2P m ()
onPeerMessage nodeId =
  \case
    GetPeers -> do
      isDisconnect <- Set.member (hostName nodeId) <$> use _disconnects
      unless isDisconnect do
        peers <- filterPeers mempty mempty <$> use _peers
        lift $ sendRemote nodeId p2pCapId $ Peers peers
        newPeer nodeId

    Peers peers -> do
      PeerState{..} <- get
      let filtered = filterPeers p2pPeers p2pDisconnects peers
      forM_ filtered $ lift . doDiscover

  where
  filterPeers knownNodes knownIps =
    -- delete the caller
    Set.delete nodeId .
    -- delete inaccessible peers
    (if hostName nodeId /= "127.0.0.1"
        then Set.filter \h -> hostName h /= "127.0.0.1"
        else id) .
    -- exclude nodes
    flip Set.difference knownNodes .
    -- exclude IPs
    Set.filter (\n -> not $ Set.member (hostName n) knownIps)

newPeer :: LiftP2P m => NodeId -> P2P m ()
newPeer nodeId = do
  peers <- use _peers
  unless (Set.member nodeId peers) do

    _peers %= Set.insert nodeId
    lift do
      sendUI $ UI_Peers $ length peers + 1
      monitorRemote nodeId $ liftP2P $ dropPeer nodeId
      sendRemote nodeId p2pCapId GetPeers

dropPeer :: LiftP2P m => NodeId -> P2P m ()
dropPeer nodeId = do
  _disconnects %= Set.insert (hostName nodeId)
  peers <- _peers <%= Set.delete nodeId
  lift $ sendUI $ UI_Peers $ length peers

