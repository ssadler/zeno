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
  , sourceChain :: KMDSource
  , destChain :: ETHDest
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
        let sourceChain = KMDSource kmdChainSymbol
        let destChain = ETHDest ethChainId "ROPSTEN" notarisationsContract
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




class NotarisationData nota where
  foreignHeight :: nota -> Word32
  notarisedHeight :: nota -> Word32

instance NotarisationData KomodoNotarisationReceipt where
  notarisedHeight = norBlockNumber . unKNR
  foreignHeight = parseForeignHeight . norForeignRef . unKNR
    where parseForeignHeight = either murphy id . decode . unFixed

instance NotarisationData EthNotarisationData where
  notarisedHeight = noeLocalHeight
  foreignHeight = noeForeignHeight
