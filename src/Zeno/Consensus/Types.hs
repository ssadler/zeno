{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Zeno.Consensus.Types where

import qualified Data.Map as Map
import qualified Data.Set as Set

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Zeno.Process
import           Network.Ethereum.Crypto
import           GHC.Generics (Generic)
import           Zeno.Prelude
import           Data.Binary
import           UnliftIO

import           Zeno.Process


type Peers = Set.Set NodeId
data PeerState = PeerState
  { p2pPeers :: TVar Peers
  , p2pPeerNotifier :: PeerNotifier
  }

data PeerNotifierMessage =
    SubscribeNewPeers Int (NodeId -> IO ())
  | UnsubscribeNewPeers Int
  | NewPeer NodeId

data PeerNotifier = PeerNotifier
  { pnProc :: Process PeerNotifierMessage
  , pnCount :: TVar Int
  }

data ConsensusNode = ConsensusNode
  { cpNode :: Node
  , cpPeers :: PeerState
  }
instance Has Node ConsensusNode where has = cpNode 
instance Has PeerState ConsensusNode where has = cpPeers

data ConsensusContext = ConsensusContext
  { ccNode   :: ConsensusNode
  , ccParams :: ConsensusParams
  }
instance Has Node ConsensusContext where has = has . ccNode
instance Has PeerState ConsensusContext where has = has . ccNode
instance Has ConsensusParams ConsensusContext where has = ccParams


data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show, Generic)

instance Binary a => Binary (Ballot a)

type Authenticated a = (CompactRecSig, a)
type Inventory a = Map Address (CompactRecSig, a)
type Collect a = Inventory a -> Consensus Bool
type Sendable a = (Binary a, Typeable a)

unInventory :: Inventory a -> [Ballot a]
unInventory inv = [Ballot a s o | (a, (s, o)) <- Map.toAscList inv]

data StepMessage a =
    InventoryIndex Integer
  | GetInventory Integer
  | InventoryData (Inventory a)
  deriving (Generic, Show)

instance Sendable a => Binary (StepMessage a)



-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members' :: [Address]
  , ident' :: EthIdent
  , timeout' :: Timeout
  , mtopic :: TVar Msg
  }

-- Monad ----------------------------------------------------------------------

type Topic = Msg

type Consensus = Zeno ConsensusContext

data ConsensusException = ConsensusTimeout String
                        | ConsensusMischief String
  deriving (Show)
instance Exception ConsensusException


withTimeout :: Int -> Consensus a -> Consensus a
withTimeout t =
  local $
    \c -> c { ccParams = (ccParams c) { timeout' = t } }
