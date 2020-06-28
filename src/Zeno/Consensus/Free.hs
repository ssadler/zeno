
module Zeno.Consensus.Free where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Skeleton

import Data.ByteString (ByteString)

import Network.Ethereum

import Zeno.Consensus.Types
import Zeno.Process.Types


type ConsensusStep m = Skeleton (ConsensusStepI m)

data ConsensusStepI m x where
  RegisterOnNewPeerFree    :: (NodeId -> ConsensusStep m ()) -> ConsensusStepI m ()
  --ReceiveFree              :: ConsensusStepI m (Maybe (AuthenticatedStepMessage i))
  ReceiveFree              :: ConsensusStepI m (Maybe (RemoteMessage ByteString))
  GetPeersFree             :: ConsensusStepI m [NodeId]
  GetIdentFree             :: ConsensusStepI m EthIdent
  SendRemoteFree           :: BallotData a => NodeId -> ProcessId -> a -> ConsensusStepI m ()
  ConsensusStepLift        :: m a -> ConsensusStepI m a

instance MonadIO m => MonadIO (ConsensusStep m) where
  liftIO = bone . ConsensusStepLift . liftIO

instance MonadLogger m => MonadLogger (ConsensusStep m) where
  monadLoggerLog a b c d = bone $ ConsensusStepLift $ monadLoggerLog a b c d
