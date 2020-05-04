{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zeno.Notariser.Types where

import Data.Aeson (FromJSON)

import Network.Ethereum.Crypto.Address
import Network.Komodo
import Network.Bitcoin
import Network.Ethereum
import Zeno.Consensus

import Zeno.Prelude


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


newtype Members = Members { unMembers :: [Address] }
  deriving (FromJSON, Show)

data NotariserConfig = NotariserConfig
  { notariser :: EthNotariser
  , notarisationsContract :: Address
  , members :: Members
  , threshold :: Int
  , kmdAlias :: String
  }

instance Has EthNotariser  NotariserConfig where has = notariser
instance Has GethConfig    NotariserConfig where has = gethConfig . has
instance Has BitcoinConfig NotariserConfig where has = getKomodoConfig . has
instance Has ConsensusNode NotariserConfig where has = getNode . has
instance Has KomodoIdent   NotariserConfig where has = deriveKomodoIdent . getSecret . has
instance Has EthIdent      NotariserConfig where has = deriveEthIdent . getSecret . has
