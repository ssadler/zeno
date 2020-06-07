{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Strict #-}

module Zeno.Consensus.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Serialize

import qualified Haskoin as H

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Ethereum.Crypto
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

getRoundEntropy :: Consensus Bytes32
getRoundEntropy = asks ccEntropy

getStepEntropy :: Consensus Bytes32
getStepEntropy = do
  (,) <$> getRoundEntropy <*> getStepNum <&> sha3b . encode

getStepNum :: Consensus StepNum
getStepNum = asks ccStepNum >>= readTVarIO

incMajorStepNum :: Consensus VarInt
incMajorStepNum = do
  t <- asks ccStepNum
  atomically $ modifyTVar t $ (major +~ 1) . (minor .~ 0)
  readTVarIO t <&> view major

incMinorStepNum :: Consensus ()
incMinorStepNum = do
  t <- asks ccStepNum
  atomically $ modifyTVar t $ minor +~ 0


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

data StepMessage a = StepMessage
  { msgIndex   :: Integer
  , msgRequest :: Integer
  , msgInvData :: Inventory a
  } deriving (Generic, Show)

instance Serialize a => Serialize (StepMessage a)
data WrappedStepMessage i = WrappedStepMessage CompactRecSig (Maybe StepNum) (StepMessage i)
  deriving (Generic)
instance Serialize i => Serialize (WrappedStepMessage i)

type AuthenticatedStepMessage i = RemoteMessage (WrappedStepMessage i)

-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members'           :: [Address]
  , ident'             :: EthIdent
  , timeout'           :: Timeout
  , onProposerTimeout' :: Maybe (ProposerTimeout -> IO ())
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

type Consensus = Zeno ConsensusContext

data ConsensusTimeout = ConsensusTimeout String deriving (Show)
instance Exception ConsensusTimeout
data ConsensusMischief = ConsensusMischief Address String deriving (Show)
instance Exception ConsensusMischief

-- Do we need this?
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
