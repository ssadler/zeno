{-# LANGUAGE UndecidableInstances #-}

module Zeno.Consensus.Types where

import           Control.Monad.Reader
import           Control.Monad.Skeleton
import           Control.Monad.State

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Serialize
import           Data.Time.Clock.POSIX
import           Data.IntMap.Strict as IntMap

import qualified Haskoin as H

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data.Utils
import           GHC.Generics (Generic)
import           UnliftIO

import           Zeno.Prelude
import           Zeno.Process
import           Zeno.Consensus.P2P


-- TODO: Clean all this up and organise into sections.

data ConsensusNode m = ConsensusNode
  { cpNode :: Node
  , cpPeers :: PeerState
  , cpRunner :: RunnerProc m
  }
instance Has Node (ConsensusNode m) where has = cpNode 
instance Has PeerState (ConsensusNode m) where has = cpPeers
instance Has (RunnerProc m) (ConsensusNode m) where has = cpRunner

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: RecSig
  , bData :: a
  } deriving (Show, Generic)

type BallotData a = (Typeable a, Serialize a)

instance Serialize a => Serialize (Ballot a)

type Inventory a = Map Address (RecSig, a)

unInventory :: Inventory a -> [Ballot a]
unInventory inv = [Ballot a s o | (a, (s, o)) <- Map.toAscList inv]
toInventory ballots = Map.fromList [(a, (s, o)) | Ballot a s o <- ballots]


data StepMessage a = StepMessage
  { msgIndex   :: PackedInteger
  , msgRequest :: PackedInteger
  , msgInvData :: Inventory a
  } deriving (Eq, Generic, Show)

instance Serialize a => Serialize (StepMessage a)

data StepId = StepId
  { stRoundId   :: RoundId
  , stStepNum   :: Word8
  , stRetry     :: Word8
  } deriving (Eq, Ord, Generic)

instance Serialize StepId

instance Show StepId where
  show (StepId r n p) = "%s:%i:%i" % (show r, n, p)

-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members'     :: [Address]
  , ident'       :: EthIdent
  , timeout'     :: Timeout
  , roundTypeId  :: Word8
  }

instance Has EthIdent ConsensusParams where has = ident'

data ConsensusNetworkConfig = CNC
  { seeds :: [NodeId]
  , netConf :: NetworkConfig
  } deriving (Show)


newtype RoundProtocol = RoundProtocol Word64
  deriving Serialize via H.VarInt


-- Frontend -------------------------------------------------------------------
--

type Consensus m = ReaderT RoundData m

-- Step --
data StepInput
  = StepTick
  | StepData (RemoteMessage LazyByteString)
  | StepNewPeer NodeId

type ConsensusStep m = Skeleton (ConsensusStepI m)

data ConsensusStepI m x where
  ReceiveFree        :: ConsensusStepI m StepInput
  RegisterTickFree   :: Int -> ConsensusStepI m ()
  GetPeersFree       :: ConsensusStepI m [NodeId]
  SendRemoteFree     :: NodeId -> LazyByteString -> ConsensusStepI m ()
  ConsensusStepLift  :: m a -> ConsensusStepI m a


instance MonadIO m => MonadIO (ConsensusStep m) where
  liftIO = bone . ConsensusStepLift . liftIO

instance MonadLogger m => MonadLogger (ConsensusStep m) where
  monadLoggerLog a b c d = bone $ ConsensusStepLift $ monadLoggerLog a b c d

type RoundId = Bytes11

type Resume m = StepInput -> ConsensusStep m Void
type RoundsMap m = Map StepId (Resume m)
type MissCache = IntMap.IntMap (StepId, RemoteMessage LazyByteString)
type DelayMap m = Map POSIXTime (RunnerAction m)
type RunnerState m = (RoundsMap m, DelayMap m, MissCache)
_missCache :: Lens' (RunnerState m) MissCache
_missCache = _3
_delays :: Lens' (RunnerState m) (DelayMap m)
_delays = _2

type RunnerProc m = MVar (RunnerState m)
type ZenoRunnerBase = Zeno (Node, PeerState)
newtype RunnerAction m = RunnerAction { unRunnerAction :: Runner m () }
type Runner m = StateT (RunnerState m) m

data RoundData = RoundData
  { params       :: ConsensusParams
  , seed         :: Bytes32
  , roundId      :: RoundId
  , mutStepNum   :: IORef Word8
  , mutStepRetry :: IORef Word8
  }

type RunnerBase m = (Monad m, HasP2P m, MonadIO m, MonadLogger m)

instance Has ConsensusParams RoundData where has = params

type ZenoConsensusNode = ConsensusNode ZenoRunnerBase

data ConsensusTopicRegistered = ConsensusTopicRegistered deriving (Show)
instance Exception ConsensusTopicRegistered
data ConsensusTimeout = ConsensusTimeout deriving (Show)
instance Exception ConsensusTimeout
data ConsensusMischief = ConsensusMischief Address String deriving (Show)
instance Exception ConsensusMischief
data ConsensusInvalidProposal = ConsensusInvalidProposal String deriving (Show)
instance Exception ConsensusInvalidProposal
