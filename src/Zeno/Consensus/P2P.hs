{-# LANGUAGE KindSignatures #-}

module Zeno.Consensus.P2P
  ( startP2P
  , PeerState
  , getPeers
  , sendPeers
  , registerOnNewPeer
  -- For testing
  , PeerMsg(..)
  , peerControllerPid
  ) where

import Control.Concurrent.STM.TVar (stateTVar)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import Data.Serialize
import Data.Dynamic

import System.Posix.Signals
import UnliftIO

import Zeno.Console
import Zeno.Process
import Zeno.Prelude hiding (finally)


data P2PNode = P2PNode
  { p2pNode :: Node
  , p2pState :: PeerState
  }

type Peers = Set.Set NodeId
data PeerState = PeerState
  { p2pPeers :: TVar Peers
  , p2pPeerNotifier :: PeerNotifier
  }

data PeerNotifierMessage =
    SubscribeNewPeers Int (NodeId -> IO ())
  | UnsubscribeNewPeers Int
  | NewPeer NodeId

data PeerNotifier = PeerNotifier
  { pnProc :: Process PeerNotifierMessage
  , pnCount :: TVar Int
  }



-- * Peer-to-peer API

getPeers :: Has PeerState r => Zeno r [NodeId]
getPeers = do
  PeerState{..} <- asks has
  Set.toList <$> readTVarIO p2pPeers

sendPeers :: (Has PeerState r, Has Node r, Serialize o) => ProcessId -> o -> Zeno r ()
sendPeers pid msg = do
  peers <- getPeers
  forM_ peers $ \peer -> sendRemote peer pid msg

registerOnNewPeer :: Has PeerState r => (NodeId -> Zeno r ()) -> Zeno r ()
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
    let peersWithoutCaller = Peers $ Set.delete peer peers
    sendRemote peer peerControllerPid peersWithoutCaller
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
      let f ps = let s' = Set.insert nodeId peers in (length s', s')
      nPeers <- atomically $ stateTVar p2pPeers f
      sendUI $ UI_Peers nPeers
      monitorRemote nodeId $ dropPeer nodeId
      send pnProc $ NewPeer nodeId
      sendRemote nodeId peerControllerPid GetPeers

  dropPeer nodeId = do
    atomically do
      modifyTVar p2pPeers $ Set.delete nodeId
    readTVarIO p2pPeers >>= sendUI . UI_Peers . length



peerNotifier :: Process PeerNotifierMessage -> Zeno Node ()
peerNotifier proc = do
  fix1 mempty $
    \go !listeners -> do
      receiveWait proc >>=
        \case
          SubscribeNewPeers subId !f -> do
            go $ IntMap.insert subId f listeners
          UnsubscribeNewPeers subId -> do
            go $ IntMap.delete subId listeners
          NewPeer nodeId -> do
            liftIO do
              forM_ (IntMap.elems listeners) ($ nodeId)
            go listeners

