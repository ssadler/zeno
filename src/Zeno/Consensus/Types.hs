
module Zeno.Consensus.Types where

import           Control.Monad.Reader
import           Control.Monad.Skeleton

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Serialize

import qualified Haskoin as H

import           Control.Exception
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
  , cpRunner :: ConsensusRunner m
  }
instance Has Node (ConsensusNode m) where has = cpNode 
instance Has PeerState (ConsensusNode m) where has = cpPeers
instance Has (ConsensusRunner m) (ConsensusNode m) where has = cpRunner

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: RecSig
  , bData :: a
  } deriving (Show, Generic)

type BallotData a = (Typeable a, Serialize a)

instance Serialize a => Serialize (Ballot a)

type Authenticated a = (RecSig, a)
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
  , stStepNum   :: VarInt
  } deriving (Show, Generic)

instance Serialize StepId

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


-- Monad ----------------------------------------------------------------------
--

type Collect i m o = TMVar (Inventory i) -> Consensus m o

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

data ConsensusControlMsg m
  = NewStep RoundId (ConsensusStep m Void)
  | NewPeer NodeId
  | GetRoundSize RoundId (Int -> m ())
  | PeerMessage (RemoteMessage LazyByteString)
  | ReleaseRound RoundId
  | DumpStatus

type ConsensusRunner m = Process (ConsensusControlMsg m)
type ZenoRunnerBase = Zeno (Node, PeerState)

data RoundData m = RoundData
  { manager :: ConsensusRunner m
  , params  :: ConsensusParams
  , seed    :: Bytes32
  , roundId :: RoundId
  }

instance Has ConsensusParams (RoundData m) where has = params

type Consensus m a = ReaderT (RoundData (ConsensusBackend m)) m a

class Monad m => ConsensusFrontend m where
  type ConsensusBackend m :: * -> *

instance ConsensusFrontend (Zeno r) where
  type ConsensusBackend (Zeno r) = ZenoRunnerBase

type ZenoConsensusNode = ConsensusNode ZenoRunnerBase

data ConsensusTimeout = ConsensusTimeout deriving (Show)
instance Exception ConsensusTimeout
data ConsensusMischief = ConsensusMischief Address String deriving (Show)
instance Exception ConsensusMischief
data ConsensusInvalidProposal = ConsensusInvalidProposal String deriving (Show)
instance Exception ConsensusInvalidProposal
