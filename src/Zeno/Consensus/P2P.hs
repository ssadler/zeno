{-# LANGUAGE KindSignatures #-}

module Zeno.Consensus.P2P
  ( HasP2P(..)
  , ZenoProcess(..)
  , PeerNotifier
  , PeerState
  , startP2P
  , getPeers
  , sendPeers
  , onNewPeer
  ) where

import Network.Distributed

import Control.Monad
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary
import Data.Dynamic

import Zeno.Prelude

import System.Posix.Signals
import UnliftIO

import Debug.Trace

-- * Peer-to-peer API

class (MonadIO p, Monad p, MonadLogger p) => HasP2P p where
  getPeerState :: p PeerState

getPeers :: HasP2P p => p [NodeId]
getPeers = do
  PeerState peers <- getPeerState
  Set.toList <$> readTVarIO peers

sendPeers :: (MonadProcess i m p, HasP2P (p i m), Binary o) => ProcessId -> o -> p i m ()
sendPeers pid msg = do
  peers <- getPeers
  forM_ peers $ \peer -> sendRemote peer pid msg

onNewPeer :: MonadProcess i m p => (NodeId -> PeerNotifier ()) -> p i m ()
onNewPeer act = do
  pid <- getMyPid
  send peerNotifierPid $ SubscribeNewPeers pid act

-- * Types

type Peers = Set.Set NodeId

data PeerState = PeerState { p2pPeers :: TVar Peers }

-- * PeerController

type PeerController = ZenoProcess ProcessData RemotePacket IO

startP2P
  :: Node
  -> [NodeId]
  -> IO PeerState
startP2P node seeds = do
  ps <- PeerState <$> newTVarIO mempty
  nodeSpawnNamed node peerControllerPid $
    runZenoProcess $ peerController ps seeds
  nodeSpawnNamed node peerNotifierPid $
    runZenoProcess peerNotifier
  installHandler sigUSR1 (Catch $ dumpPeers ps) Nothing
  pure ps
  where

  dumpPeers PeerState{..} = do
    peers <- atomically $ readTVar p2pPeers
    runZeno () $ do
      logInfo "Got signal USR1"
      forM_ peers $ \p ->
        logInfo $ show p


peerControllerPid :: ProcessId
peerControllerPid = serviceId "peerController"


data PeerMsg =
    GetPeers
  | Peers Peers
  deriving (Generic)

instance Binary PeerMsg


peerController :: PeerState -> [NodeId] -> PeerController ()
peerController state@PeerState{..} seeds = do
  forever do
    mapM_ doDiscover seeds
    receiveDuringS 60 $
      withRemoteMessage $
        \peer ->
           \case 
             GetPeers -> do
               peers <- readTVarIO p2pPeers
               sendRemote peer peerControllerPid $ Peers peers
               newPeer state peer

             (Peers peers) -> do
               known <- readTVarIO $ p2pPeers
               mapM_ doDiscover $ Set.toList $ Set.difference peers known

  where
  doDiscover nodeId = do
    sendRemote nodeId peerControllerPid GetPeers

  newPeer :: PeerState -> NodeId -> PeerController ()
  newPeer state@(PeerState{..}) peer = do
    peers <- readTVarIO p2pPeers
    unless (Set.member peer peers) do
      logDebug $ "New peer: " ++ show peer
      atomically $ writeTVar p2pPeers $ Set.insert peer peers
      monitorRemote peer do
        logDebug $ "Peer disconnect: " ++ show peer
        atomically $ modifyTVar p2pPeers $ Set.delete peer
      send peerNotifierPid $ NewPeer peer
      sendRemote peer peerControllerPid GetPeers


peerNotifierPid = serviceId "peerNotifier"

type PeerNotifier = ZenoProcess ProcessData PeerNotifierMessage IO

data PeerNotifierMessage =
    SubscribeNewPeers ProcessId (NodeId -> PeerNotifier ())
  | Unsubscribe ProcessId
  | NewPeer NodeId

peerNotifier :: PeerNotifier ()
peerNotifier = do
  me <- getMyPid
  fix1 mempty $
    \go listeners -> do
      receiveWait >>=
        \case
          SubscribeNewPeers pid f -> do
            monitorLocal pid $ send me $ Unsubscribe pid
            go $ Map.insert pid f listeners
          Unsubscribe pid -> do
            go $ Map.delete pid listeners
          NewPeer nodeId -> do
            spawn do
              forM_ (Map.toList listeners) $ \(pid, act) -> act nodeId
            go listeners


fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
