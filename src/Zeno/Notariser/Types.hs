{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zeno.Notariser.Types where

import qualified Haskoin as H

import Data.Serialize

import Network.Ethereum.Crypto.Address
import Network.Komodo
import Network.Bitcoin
import Network.Ethereum
import Network.Ethereum.Transaction

import Zeno.Consensus
import Zeno.Data.Aeson
import Zeno.Prelude
import Zeno.EthGateway
import Zeno.Notariser.Targets

import UnliftIO


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


data AbstractNotariserConfig source dest = NotariserConfig
  { members :: [Address]
  , threshold :: Int
  , consensusTimeout :: Int
  , sourceChain :: source
  , destChain :: dest
  } deriving (Show, Eq)

type NotariserConfig = AbstractNotariserConfig KMDSource ETHDest

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
        ethBlockInterval      <- o .: "ethBlockInterval" <|> pure 5
        let sourceChain = KMDSource kmdChainSymbol kmdNotarySigs kmdBlockInterval
        let destChain = ETHDest ethChainId "ROPSTEN" notarisationsContract ethNotariseGas ethBlockInterval
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





data KMDSource = KMDSource
  { kmdSymbol :: String
  , kmdNotarySigs :: Int
  , kmdBlockInterval :: Word32
  } deriving (Show, Eq)

instance BlockchainConfig KMDSource where
  getSymbol = kmdSymbol
  getNotarisationBlockInterval = kmdBlockInterval

instance Has BitcoinConfig r => BlockchainAPI KMDSource (Zeno r) where
  getHeight KMDSource{..} = bitcoinGetHeight

instance Has BitcoinConfig r => SourceChain KMDSource (Zeno r) where
  type (ChainNotarisationReceipt KMDSource) = KomodoNotarisationReceipt
  getLastNotarisationReceipt KMDSource{..} = kmdGetLastNotarisationData kmdSymbol


data ETHDest = ETHDest
  { ethChainId :: ChainId
  , ethSymbol :: String
  , ethNotarisationsContract :: Address
  , ethNotariseGas :: Integer
  , ethBlockInterval :: Word32
  } deriving (Show, Eq)

instance BlockchainConfig ETHDest where
  getSymbol = ethSymbol
  getNotarisationBlockInterval = ethBlockInterval

instance BlockchainAPI ETHDest (Zeno r) where
  getHeight ETHDest{..} = error "ETHDest waitheight"

instance Has GethConfig r => DestChain ETHDest (Zeno r) where
  type (ChainNotarisation ETHDest) = EthNotarisationData
  getLastNotarisationAndSequence ETHDest{..} = do
    ethGetLastNotarisationAndSequence ethNotarisationsContract





instance NotarisationReceipt KomodoNotarisationReceipt where
  receiptHeight = norBlockNumber . unKNR

instance Notarisation EthNotarisationData where
  foreignHeight = noeForeignHeight


