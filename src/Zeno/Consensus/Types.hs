{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Zeno.Consensus.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Serialize

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Ethereum.Crypto
import           GHC.Generics (Generic)
import           UnliftIO

import           Zeno.Data.Aeson
import           Zeno.Data.FixedBytes
import           Zeno.Prelude
import           Zeno.Process
import           Zeno.Consensus.P2P


-- TODO: Clean all this up and organise into sections.


data ConsensusNode = ConsensusNode
  { cpNode :: Node
  , cpPeers :: PeerState
  }
instance Has Node ConsensusNode where has = cpNode 
instance Has PeerState ConsensusNode where has = cpPeers

data ConsensusContext = ConsensusContext
  { ccNode     :: ConsensusNode
  , ccParams   :: ConsensusParams
  , ccEntropy  :: Bytes32
  , ccStepNum  :: TVar StepNum
  }
instance Has ConsensusNode ConsensusContext where has = ccNode
instance Has Node ConsensusContext where has = has . ccNode
instance Has PeerState ConsensusContext where has = has . ccNode
instance Has ConsensusParams ConsensusContext where has = ccParams
instance Has EthIdent ConsensusContext where has = has . ccParams

type StepNum = (Int, (Maybe Int))

getStepNum :: Consensus StepNum
getStepNum = asks ccStepNum >>= readTVarIO

incStepNum :: Consensus Int
incStepNum = do
  t <- asks ccStepNum
  (major, _) <- readTVarIO t
  let next = major + 1
  atomically $ writeTVar t (next, Nothing)
  pure next

incMinorStepNum :: Consensus Int
incMinorStepNum = do
  t <- asks ccStepNum
  (major, minor) <- readTVarIO t
  let next = maybe 1 (+1) minor
  atomically $ writeTVar t (major, Just next)
  pure next


type RoundId = Bytes6

getRoundId :: Consensus RoundId
getRoundId = do
  entropy <- asks ccEntropy
  pure $ toFixedR $ unFixed entropy

getMyAddress :: Consensus Address
getMyAddress = asks $ ethAddress . ident' . ccParams


data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show, Generic)

type BallotData a = (Typeable a, Serialize a)

instance Serialize a => Serialize (Ballot a)

type Authenticated a = (CompactRecSig, a)
type Inventory a = Map Address (CompactRecSig, a)
type Collect a b = Inventory a -> Consensus (Maybe b)

unInventory :: Inventory a -> [Ballot a]
unInventory inv = [Ballot a s o | (a, (s, o)) <- Map.toAscList inv]

data StepMessage a =
    InventoryIndex Integer
  | GetInventory Integer
  | InventoryData (Inventory a)
  deriving (Generic, Show)

instance Serialize a => Serialize (StepMessage a)

type AuthenticatedStepMessage i = RemoteMessage (CompactRecSig, StepMessage i)

-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members'           :: [Address]
  , ident'             :: EthIdent
  , timeout'           :: Timeout
  , onProposerTimeout' :: Maybe (ProposerTimeout -> IO ())
  }

instance Has EthIdent ConsensusParams where has = ident'

data ConsensusNetworkConfig = CNC
  { seeds :: [NodeId]
  , netConf :: NetworkConfig
  } deriving (Show)


-- Monad ----------------------------------------------------------------------

type Consensus = Zeno ConsensusContext

data ConsensusTimeout = ConsensusTimeout String deriving (Show)
instance Exception ConsensusTimeout
data ConsensusMischief = ConsensusMischief Address String deriving (Show)
instance Exception ConsensusMischief


withTimeout :: Int -> Consensus a -> Consensus a
withTimeout t =
  local $
    \c -> c { ccParams = (ccParams c) { timeout' = t } }


--------------------------------------------------------------------------------
-- Stats types
--------------------------------------------------------------------------------

data ProposerTimeout = ProposerTimeout
  { proposer :: Address
  , roundId  :: RoundId
  , stepNum  :: StepNum
  } deriving (Show, Generic)
    deriving Serialize via (SerializeAeson ProposerTimeout)
 
instance ToJSON ProposerTimeout
instance FromJSON ProposerTimeout

newtype ProposerSequence = ProposerSequence Int
  deriving (Show, Eq, Ord, Num)
