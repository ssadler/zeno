
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


data StepNum = StepNum
  { stRound  :: VarInt
  , stMajor  :: VarInt
  , stMinor  :: VarInt
  } deriving (Show, Generic)

instance Serialize StepNum

instance ToJSON StepNum where
  toJSON (StepNum a b c) = toJSON (a, b, c)

instance FromJSON StepNum where
  parseJSON val = do
    (a, b, c) <- parseJSON val
    pure $ StepNum a b c

makeLensesWith abbreviatedFields ''StepNum

data ConsensusNode = ConsensusNode
  { cpNode :: Node
  , cpPeers :: PeerState
  , cpRunner :: ConsensusRunner
  }
instance Has Node ConsensusNode where has = cpNode 
instance Has PeerState ConsensusNode where has = cpPeers
instance Has ConsensusRunner ConsensusNode where has = cpRunner

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
  } deriving (Generic, Show)

instance Serialize a => Serialize (StepMessage a)
data WrappedStepMessage i = WrappedStepMessage RecSig (Maybe StepNum) (StepMessage i)
  deriving (Generic)
instance Serialize i => Serialize (WrappedStepMessage i)

type AuthenticatedStepMessage i = RemoteMessage (WrappedStepMessage i)

-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members'           :: [Address]
  , ident'             :: EthIdent
  , timeout'           :: Timeout
  , roundTypeId        :: VarInt
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
  | StepData (RemoteMessage ByteString)
  | StepNewPeer NodeId

type ConsensusStep m = Skeleton (ConsensusStepI m)

data ConsensusStepI m x where
  ReceiveFree        :: ConsensusStepI m StepInput
  RegisterTickFree   :: Int -> ConsensusStepI m ()
  GetPeersFree       :: ConsensusStepI m [NodeId]
  SendRemoteFree     :: BallotData a => NodeId -> a -> ConsensusStepI m ()
  ConsensusStepLift  :: m a -> ConsensusStepI m a

instance MonadIO m => MonadIO (ConsensusStep m) where
  liftIO = bone . ConsensusStepLift . liftIO

instance MonadLogger m => MonadLogger (ConsensusStep m) where
  monadLoggerLog a b c d = bone $ ConsensusStepLift $ monadLoggerLog a b c d

type RoundId = Bytes5

data ConsensusControlMsg
  = NewStep RoundId (ConsensusStep (Zeno (Node, PeerState)) Void)
  | NewPeer NodeId
  | GetRoundSize RoundId (MVar Int)
  | PeerMessage (RemoteMessage ByteString)
  | ReleaseRound RoundId

type ConsensusRunner = Process ConsensusControlMsg
type ConsensusRunnerBase = Zeno (Node, PeerState)

data RoundData = RoundData
  { manager :: ConsensusRunner
  , seed    :: Bytes32
  , params  :: ConsensusParams
  , steps   :: [ConsensusStep ConsensusRunnerBase ()]
  }

instance Has ConsensusParams RoundData where has = params

type Consensus m a = ReaderT RoundData m a

data ConsensusTimeout = ConsensusTimeout deriving (Show)
instance Exception ConsensusTimeout
data ConsensusMischief = ConsensusMischief Address String deriving (Show)
instance Exception ConsensusMischief
data ConsensusInvalidProposal = ConsensusInvalidProposal String deriving (Show)
instance Exception ConsensusInvalidProposal
