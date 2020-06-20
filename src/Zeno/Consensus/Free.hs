
module Zeno.Consensus.Free where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Skeleton

import Network.Ethereum

import Zeno.Consensus.Types
import Zeno.Process.Types


type ConsensusStep i m = Skeleton (ConsensusStepI i m)

data ConsensusStepI i m x where
  RegisterOnNewPeerFree    :: (NodeId -> ConsensusStep i m ()) -> ConsensusStepI i m ()
  ReceiveFree              :: ConsensusStepI i m (Maybe (AuthenticatedStepMessage i))
  GetPeersFree             :: ConsensusStepI i m [NodeId]
  GetIdentFree             :: ConsensusStepI i m EthIdent
  SendRemoteFree           :: BallotData a => NodeId -> ProcessId -> a -> ConsensusStepI i m ()
  YieldFree                :: Inventory i -> ConsensusStepI i m ()
  ConsensusStepLift        :: m a -> ConsensusStepI i m a

instance MonadIO m => MonadIO (ConsensusStep i m) where
  liftIO = bone . ConsensusStepLift . liftIO

instance MonadLogger m => MonadLogger (ConsensusStep i m) where
  monadLoggerLog a b c d = bone $ ConsensusStepLift $ monadLoggerLog a b c d
