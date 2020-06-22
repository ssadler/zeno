{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Strict #-}

module Zeno.Consensus.Types where

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
  }
instance Has Node ConsensusNode where has = cpNode 
instance Has PeerState ConsensusNode where has = cpPeers

data ConsensusContext = ConsensusContext
  { ccNode     :: ConsensusNode
  , ccParams   :: ConsensusParams
  , ccSeed     :: Bytes32
  , ccStepNum  :: TVar StepNum        -- TODO: Not so sure it's smart for the step
                                      -- to have access to this, since it's mutable
                                      -- and shared.
  }

instance Has ConsensusNode ConsensusContext where has = ccNode
instance Has Node ConsensusContext where has = has . ccNode
instance Has PeerState ConsensusContext where has = has . ccNode
instance Has ConsensusParams ConsensusContext where has = ccParams
instance Has EthIdent ConsensusContext where has = has . ccParams

getRoundSeed :: Consensus Bytes32
getRoundSeed = asks ccSeed

getStepSeed :: Consensus Bytes32
getStepSeed = do
  (,) <$> getRoundSeed <*> getStepNum <&> sha3b . encode

getStepNum :: Consensus StepNum
getStepNum = asks ccStepNum >>= readTVarIO

incMajorStepNum :: Consensus VarInt
incMajorStepNum = do
  t <- asks ccStepNum
  atomically $ modifyTVar t $ (major +~ 1) . (minor .~ 0)
  readTVarIO t <&> view major

incMinorStepNum :: Consensus VarInt
incMinorStepNum = do
  t <- asks ccStepNum
  atomically $ modifyTVar t $ minor +~ 1
  readTVarIO t <&> view minor


type RoundId = Bytes6

getRoundId :: Consensus RoundId
getRoundId = do
  seed <- asks ccSeed
  pure $ toFixedR $ fromFixed seed

getMyAddress :: Consensus Address
getMyAddress = asks $ ethAddress . ident' . ccParams


data Ballot a = Ballot
  { bMember :: Address
  , bSig :: RecSig
  , bData :: a
  } deriving (Show, Generic)

type BallotData a = (Typeable a, Serialize a)

instance Serialize a => Serialize (Ballot a)

type Authenticated a = (RecSig, a)
type Inventory a = Map Address (RecSig, a)
type Collect i o = Inventory i -> Consensus (Maybe o)

unInventory :: Inventory a -> [Ballot a]
unInventory inv = [Ballot a s o | (a, (s, o)) <- Map.toAscList inv]


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

withTimeout :: Int -> Consensus a -> Consensus a
withTimeout t = 
  local \c -> c { ccParams = (ccParams c) { timeout' = t } }


-- Monad ----------------------------------------------------------------------

type Consensus = Zeno ConsensusContext

data ConsensusTimeout = ConsensusTimeout deriving (Show)
instance Exception ConsensusTimeout
data ConsensusMischief = ConsensusMischief Address String deriving (Show)
instance Exception ConsensusMischief
data ConsensusInvalidProposal = ConsensusInvalidProposal String deriving (Show)
instance Exception ConsensusInvalidProposal
