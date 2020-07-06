{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Zeno.Consensus.P2P
  ( HasP2P(..)
  , startP2P
  , PeerState
  , sendPeers
  , registerOnNewPeer
  -- For testing
  , PeerMsg(..)
  , peerCapabilityId
  ) where

import Control.Concurrent.STM.TVar (stateTVar)
import Data.Bits
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

import UnliftIO

import Zeno.Console
import Zeno.Process
import Zeno.Prelude hiding (finally)
import Zeno.Signal


data P2PNode = P2PNode
  { p2pNode :: Node
  , p2pState :: PeerState
  }

type Peers = Set.Set NodeId

data PeerState = PeerState
  { p2pPeers :: TVar Peers
  , p2pPeerNotifier :: MVar (IntMap.IntMap (NodeId -> IO ()))
  }


-- * Peer-to-peer API

class (Monad m, HasNode m) => HasP2P m where
  getPeers :: m [NodeId]

instance (HasNode (Zeno r), Has PeerState r) => HasP2P (Zeno r) where
  getPeers = do
    PeerState{..} <- asks has
    Set.toList <$> readTVarIO p2pPeers

sendPeers :: (HasP2P m, Serialize o) => CapabilityId -> o -> m ()
sendPeers capid msg = do
  peers <- getPeers
  forM_ peers $ \peer -> sendRemote peer capid msg

registerOnNewPeer :: Has PeerState r => (NodeId -> Zeno r ()) -> Zeno r ()
registerOnNewPeer cb = do
  PeerState{..} <- asks has
  rio <- askRunInIO
  void $ allocate
    do
      liftIO do
        modifyMVar p2pPeerNotifier \m -> do
          let i = 1 + maybe 0 fst (IntMap.lookupMax m)
          pure (IntMap.insert i (rio . cb) m, i)

    \i -> modifyMVar_ p2pPeerNotifier $ pure . IntMap.delete i


startP2P :: [NodeId] -> Zeno Node PeerState
startP2P seeds = do
  p2pPeers <- newTVarIO mempty
  p2pPeerNotifier <- newMVar mempty
  let state = PeerState{..}
  _ <- startPeerController state seeds
  installSignalHandler sigUSR1 $ dumpPeers state
  pure state
  where
  dumpPeers PeerState{..} = do
    peers <- atomically $ readTVar p2pPeers
    logInfo "Peers:"
    forM_ peers $ logInfo . show


peerCapabilityId :: CapabilityId
peerCapabilityId = 1


data PeerMsg = GetPeers | Peers Peers
  deriving (Eq, Show, Generic)

instance Serialize PeerMsg


startPeerController :: PeerState -> [NodeId] -> Zeno Node ()
startPeerController state@PeerState{..} seeds = do
  myIp <- getMyIp
  void $ spawn "peerController" \chan -> do
    registerCapability 1 $ send chan
    forever do
      mapM_ doDiscover seeds
      receiveDuringS (procMbox chan) 60 $
        withRemoteMessage
          \peer msg -> do
            when (hostName peer /= myIp) do
              handle peer msg
  where
  handle peer GetPeers = do
    peers <- filterPeers <$> readTVarIO p2pPeers
    sendRemote peer peerCapabilityId $ Peers peers
    newPeer peer
    where
    filterPeers =
      Set.delete peer .
       if hostName peer == "127.0.0.1"
          then id
          else Set.filter (\h -> hostName h /= "127.0.0.1")

  handle peer (Peers peers) = do
    known <- readTVarIO $ p2pPeers
    mapM_ doDiscover $ Set.toList $ Set.difference peers known

  doDiscover peer = do
    myIp <- getMyIp
    when (hostName peer /= myIp) do
      sendRemote peer peerCapabilityId GetPeers

  newPeer :: NodeId -> Zeno Node ()
  newPeer nodeId = do
    peers <- readTVarIO p2pPeers

    unless (Set.member nodeId peers) do
      let f ps = let s' = Set.insert nodeId peers in (length s', s')
      nPeers <- atomically $ stateTVar p2pPeers f
      sendUI $ UI_Peers nPeers
      monitorRemote nodeId $ dropPeer nodeId
      notifyNewPeer nodeId
      sendRemote nodeId peerCapabilityId GetPeers

  notifyNewPeer nodeId = do
    liftIO $ forkIO do
      cbs <- readMVar p2pPeerNotifier
      forM_ cbs ($ nodeId)

  dropPeer nodeId = do
    atomically do
      modifyTVar p2pPeers $ Set.delete nodeId
    readTVarIO p2pPeers >>= sendUI . UI_Peers . length
