{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}

module Zeno.Consensus.Types where

import qualified Data.Map as Map

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.Distributed
import           Network.Ethereum.Crypto
import           GHC.Generics (Generic)
import           Zeno.Prelude
import           Data.Binary
import           UnliftIO

import           Zeno.Consensus.P2P


data ConsensusNode = ConsensusNode
  { node :: Node
  , peerState :: PeerState
  }

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show, Generic)

instance Binary a => Binary (Ballot a)

type Authenticated a = (CompactRecSig, a)
type Inventory a = Map Address (CompactRecSig, a)
type Collect a = Timeout -> [Address] -> Consensus (Inventory a)
type Sendable a = (Binary a, Typeable a)

unInventory :: Inventory a -> [Ballot a]
unInventory inv = [Ballot a s o | (a, (s, o)) <- Map.toAscList inv]

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

data ConsensusProcess = ConsensusProcess
  { cpParams :: ConsensusParams
  , cpProc   :: ProcessData
  , cpNode   :: ConsensusNode
  }
type Consensus = Zeno ConsensusProcess

instance Process Consensus where
  procAsks f = f . cpProc <$> ask
  procWith r = zenoReader (\p -> p {cpProc = r})

instance HasP2P Consensus where
  getPeerState = asks $ peerState . cpNode

data ConsensusException = ConsensusTimeout String
                        | ConsensusMischief String
  deriving (Show)
instance Exception ConsensusException


withTimeout :: Int -> Consensus a -> Consensus a
withTimeout t =
  zenoReader $
    \ConsensusProcess{..} -> ConsensusProcess { cpParams = cpParams { timeout' = t }, .. }
