{-# LANGUAGE OverloadedStrings, RecordWildCards, MonoLocalBinds, DeriveGeneric #-}

module Zeno.Consensus.P2P
  ( NewPeer(..)
  , HasP2P(..)
  , PeerState
  , startP2P
  , getPeers
  , sendPeers
  ) where

import Network.Transport (EndPointAddress(..), Transport)
import Network.Socket (HostName, ServiceName)
import Network.Transport.TCP
import Network.Distributed

import Control.Monad
import Control.Monad.Reader

import qualified Data.Set as Set
import Data.Binary
import Data.Typeable

import Zeno.Prelude

import GHC.Generics (Generic)
import System.Posix.Signals
import UnliftIO
import UnliftIO.Concurrent


-- * Peer-to-peer API

class (MonadIO p, Monad p, MonadLogger p) => HasP2P p where
  getPeerState :: p PeerState

getPeers :: HasP2P p => p [NodeId]
getPeers = do
  PeerState peers <- getPeerState
  Set.toList <$> readTVarIO peers

sendPeers :: (Process p, HasP2P p, Binary a) => ProcessId -> a -> p ()
sendPeers pid msg = do
  peers <- getPeers
  forM_ peers $ \peer -> sendRemote peer pid msg


-- * Types

type Peers = Set.Set NodeId

data PeerState = PeerState { p2pPeers :: TVar Peers }

type PeerController = Zeno ProcessData

instance Process (Zeno ProcessData) where
  procAsks = asks
  procWith pd = zenoReader (const pd)




peerControllerPid :: ProcessId
peerControllerPid = serviceId "peerController"

startP2P
  :: Node
  -> [NodeId]
  -> IO PeerState
startP2P node seeds = do
  ps <- PeerState <$> newTVarIO mempty
  nodeSpawnNamed node peerControllerPid $
    \pd -> runZeno pd $ peerController ps seeds
  installHandler sigUSR1 (Catch $ dumpPeers ps) Nothing
  pure ps

  where
  peerController :: PeerState -> [NodeId] -> PeerController ()
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
                 mapM_ doDiscover $ Set.toList $ Set.difference peers known



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
doDiscover :: NodeId -> PeerController ()
doDiscover nodeId =
  sendRemote nodeId peerControllerPid Hello


-- 2: When there's a request to share peers
onPeerHello :: PeerState -> NodeId -> PeerController ()
onPeerHello s@PeerState{..} peer = do
  peers <- readTVarIO p2pPeers
  sendRemote peer peerControllerPid peers
  newPeer s peer


newPeer :: PeerState -> NodeId -> PeerController ()
newPeer state@(PeerState{..}) peer = do
  peers <- readTVarIO p2pPeers
  unless (Set.member peer peers) do
    logDebug $ "New peer: " ++ show peer
    atomically $ writeTVar p2pPeers $ Set.insert peer peers
    monitorRemote peer dropPeer
    -- nsend peerListenerService $ NewPeer $ processNodeId pid -- TODO
    sendRemote peer peerControllerPid Hello

  where
  dropPeer = do
    logDebug $ "Peer disconnect: " ++ show peer
    atomically $ modifyTVar p2pPeers $ Set.delete peer


-- 
-- 
-- 
-- 
-- --
-- 
newtype NewPeer = NewPeer NodeId
  deriving (Binary)
