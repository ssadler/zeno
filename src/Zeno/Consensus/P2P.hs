{-# LANGUAGE KindSignatures #-}

module Zeno.Consensus.P2P
  ( startP2P
  , getPeers
  , sendPeers
  , registerOnNewPeer
  -- For testing
  , PeerMsg(..)
  , peerControllerPid
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Data.Serialize
import Data.Dynamic

import System.Posix.Signals
import UnliftIO

import Zeno.Console
import Zeno.Consensus.Types
import Zeno.Process
import Zeno.Prelude hiding (finally)


data P2PNode = P2PNode
  { p2pNode :: Node
  , p2pState :: PeerState
  }

-- * Peer-to-peer API

getPeers :: Consensus [NodeId]
getPeers = do
  PeerState{..} <- asks has
  Set.toList <$> readTVarIO p2pPeers

sendPeers :: Serialize o => ProcessId -> o -> Consensus ()
sendPeers pid msg = do
  peers <- getPeers
  forM_ peers $ \peer -> sendRemote peer pid msg

registerOnNewPeer :: (NodeId -> Consensus ()) -> Consensus ()
registerOnNewPeer cb = do
  PeerNotifier{..} <- asks $ p2pPeerNotifier . has

  (_, subId) <-
    allocate (readTVarIO pnCount)
             (send pnProc . UnsubscribeNewPeers)

  UnliftIO unliftIO <- askUnliftIO
  atomically do
    writeTVar pnCount $ subId + 1
    sendSTM pnProc $ SubscribeNewPeers subId $ unliftIO . cb


-- * Types

-- * Consensus


startP2P :: [NodeId] -> Zeno Node PeerState
startP2P seeds = do
  p2pPeers <- newTVarIO mempty
  pnCount <- newTVarIO 0
  pnProc <- spawn "peerNotifier" peerNotifier
  let p2pPeerNotifier = PeerNotifier{..}
  let state = PeerState{..}
  _ <- spawn "peerController" $ \_ -> peerController state seeds
  withRunInIO \rio ->
    installHandler sigUSR1 (Catch $ rio $ dumpPeers state) Nothing
  pure state
  where
  dumpPeers PeerState{..} = do
    peers <- atomically $ readTVar p2pPeers
    logInfo "Got signal USR1"
    forM_ peers $ \p ->
      logInfo $ show p


peerControllerPid :: ProcessId
peerControllerPid = hashServiceId "peerController"


data PeerMsg =
    GetPeers
  | Peers Peers
  deriving (Show, Generic)

instance Serialize PeerMsg


peerController :: PeerState -> [NodeId] -> Zeno Node ()
peerController state@PeerState{..} seeds = do
  recv <- subscribe peerControllerPid
  forever do
    mapM_ doDiscover seeds
    receiveDuringS recv 60 $
      withRemoteMessage handle

  where
  handle peer GetPeers = do
    peers <- readTVarIO p2pPeers
    sendRemote peer peerControllerPid $ Peers peers
    newPeer peer

  handle peer (Peers peers) = do
    known <- readTVarIO $ p2pPeers
    mapM_ doDiscover $ Set.toList $ Set.difference peers known

  doDiscover nodeId = do
    sendRemote nodeId peerControllerPid GetPeers

  newPeer :: NodeId -> Zeno Node ()
  newPeer nodeId = do
    let PeerNotifier{..} = p2pPeerNotifier
    peers <- readTVarIO p2pPeers

    unless (Set.member nodeId peers) do
      atomically do writeTVar p2pPeers $ Set.insert nodeId peers
      sendUI $ UI_NewPeer $ length peers + 1
      monitorRemote nodeId $ dropPeer nodeId
      send pnProc $ NewPeer nodeId
      sendRemote nodeId peerControllerPid GetPeers

  dropPeer nodeId = do
    join $
      atomically do
        ps <- readTVar p2pPeers
        writeTVar p2pPeers $ Set.delete nodeId ps
        pure $ sendUI $ UI_DropPeer $ length ps



peerNotifier :: Process PeerNotifierMessage -> Zeno Node ()
peerNotifier proc = do
  fix1 mempty $
    \go !listeners -> do
      receiveWait proc >>=
        \case
          SubscribeNewPeers subId f -> do
            go $ IntMap.insert subId f listeners
          UnsubscribeNewPeers subId -> do
            go $ IntMap.delete subId listeners
          NewPeer nodeId -> do
            liftIO do
              forM_ (IntMap.elems listeners) ($ nodeId)
            go listeners

