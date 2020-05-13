{-# LANGUAGE DeriveGeneric #-}

module Zeno.Consensus.Types where

import qualified Data.Map as Map

import           Control.Exception
import           Control.Monad.Reader
import           Control.Monad.State
import           Network.NQE
import           Network.Ethereum.Crypto
import           GHC.Generics (Generic)
import           Zeno.Prelude
import           Data.Binary
import           UnliftIO

data ConsensusNode = ConsensusNode { unConsensusNode :: LocalNode }

data Ballot a = Ballot
  { bMember :: Address
  , bSig :: CompactRecSig
  , bData :: a
  } deriving (Show, Generic)

instance Binary a => Binary (Ballot a)

type Authenticated a = (CompactRecSig, a)
type Inventory a = Map Address (CompactRecSig, a)
type Collect a = ReceivePort (Inventory a) -> Timeout -> [Address] -> Process (Inventory a)

unInventory :: Inventory a -> [Ballot a]
unInventory inv = [Ballot a s o | (a, (s, o)) <- Map.toAscList inv]

-- Params ---------------------------------------------------------------------

type Timeout = Int

data ConsensusParams = ConsensusParams
  { members' :: [Address]
  , getIdent' :: EthIdent
  , timeout' :: Timeout
  , mtopic :: TVar Msg
  }

-- Monad ----------------------------------------------------------------------

type Topic = Msg
type Consensus = ReaderT ConsensusParams Process

data ConsensusException = ConsensusTimeout String
                        | ConsensusMischief String
  deriving (Show)
instance Exception ConsensusException


withTimeout :: Int -> Consensus a -> Consensus a
withTimeout t = withReaderT $ \c -> c { timeout' = t }
