{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zeno.Notariser.Types where

import Zeno.Data.Aeson

import Network.Ethereum.Crypto.Address
import Network.Komodo
import Network.Bitcoin
import Network.Ethereum
import Network.Ethereum.Transaction

import Zeno.Consensus
import Zeno.Prelude

import UnliftIO


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , getNode :: ConsensusNode
  , gethConfig :: GethConfig
  , getEthGateway :: Address
  , getSecret :: SecKey
  }

instance Has GethConfig    EthNotariser where has = gethConfig
instance Has BitcoinConfig EthNotariser where has = getKomodoConfig
instance Has ConsensusNode EthNotariser where has = getNode
instance Has KomodoIdent   EthNotariser where has = deriveKomodoIdent . getSecret
instance Has EthIdent      EthNotariser where has = deriveEthIdent . getSecret


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

getConsensusParams :: NotariserConfig -> Zeno EthNotariser ConsensusParams
getConsensusParams NotariserConfig{..} = do
  ident <- asks has
  pure $ ConsensusParams members ident consensusTimeout Nothing


instance Exception ConfigException
data ConfigException = ConfigException String
  deriving (Show)

instance Exception NotariserException
data NotariserException = Inconsistent String
  deriving (Show)
