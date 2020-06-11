{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

module Zeno.Notariser.Types where

import Control.Monad.Trans.Free
import Control.Monad.Free.TH

import qualified Haskoin as H

import Network.Ethereum.Crypto.Address
import Network.Komodo
import Network.Bitcoin
import Network.Ethereum
import Network.Ethereum.Transaction

import Zeno.Consensus
import Zeno.Data.Aeson
import Zeno.Prelude
import Zeno.EthGateway

import UnliftIO


--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------


data RoundType              -- Don't go changing this willy nilly
  = KmdToEth                -- Things will break
  | EthToKmd
  | StatsToKmd
  deriving (Enum)


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , getNode :: ConsensusNode
  , gethConfig :: GethConfig
  , getEthGateway :: Address
  , getSecret :: SecKey
  , getEthIdent :: EthIdent
  , getKomodoIdent :: KomodoIdent
  }

instance Has BitcoinConfig EthNotariser where has = getKomodoConfig
instance Has ConsensusNode EthNotariser where has = getNode
instance Has GethConfig    EthNotariser where has = gethConfig
instance Has EthIdent      EthNotariser where has = getEthIdent
instance Has KomodoIdent   EthNotariser where has = getKomodoIdent


data NotariserConfig = NotariserConfig
  { members :: [Address]
  , threshold :: Int
  , notarisationsContract :: Address
  , kmdChainSymbol :: String
  , kmdNotarySigs :: Int
  , kmdBlockInterval :: Word32
  , consensusTimeout :: Int
  , ethChainId :: ChainId
  , ethNotariseGas :: Integer
  } deriving (Show, Eq)


instance FromJSON NotariserConfig where
  parseJSON =
    --- This is not a strict object because we want config additions
    --- to be backwards compatible
    withObject "NotariserConfig" $
      \o -> do
        notarisationsContract <- o .: "notarisationsContract"
        kmdChainSymbol        <- o .: "kmdChainSymbol"
        kmdNotarySigs         <- o .: "kmdNotarySigs"
        kmdBlockInterval      <- o .: "kmdBlockInterval"
        ethNotariseGas        <- o .: "ethNotariseGas"
        ethChainId            <- o .: "ethChainId"
        consensusTimeout      <- o .: "consensusTimeout" <|> pure defaultTimeout
        pure $ NotariserConfig{..}
    where
      members = uninit
      threshold = uninit
      uninit = error "NotariserConfig not fully initialized"
      defaultTimeout = 10 * 1000000


instance Exception ConfigException
data ConfigException = ConfigException String
  deriving (Show)

instance Exception NotariserException
data NotariserException = Inconsistent String
  deriving (Show)

--------------------------------------------------------------------------------
-- Notariser Free monad interface
--------------------------------------------------------------------------------

type NotariserStep m = FreeT NotariserStepF m

instance MonadLogger m => MonadLogger (NotariserStep m)

data NotariserStepF next
  = GetLastNotarisationFree     (Maybe (NotarisationOnEth, ProposerSequence) -> next)
  | GetLastNotarisationReceipt  (Maybe (Notarisation BackNotarisationData) -> next)
  | MakeNotarisationReceipt     NotarisationOnEth (NotarisationData -> next)
  | RunNotarise                 ProposerSequence Word32 (() -> next)
  | RunNotariseReceipt          ProposerSequence NotarisationData (() -> next)
  | WaitSourceHeightFree        Word32 (Word32 -> next)
  deriving (Generic, Functor)

makeFree ''NotariserStepF

data Done = Done
  deriving (Eq, Show)
