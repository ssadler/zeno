{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zeno.Notariser.Types where

import Zeno.Data.Aeson

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
  { members :: Members
  , threshold :: Int
  , notarisationsContract :: Address
  , kmdChainSymbol :: String
  , kmdNotaryInputs :: Int
  , consensusTimeout :: Int
  }

instance FromJSON NotariserConfig where
  parseJSON =
    withStrictObject "NotariserConfig" $
      \o -> do
        NotariserConfig
          uninit uninit
          <$> o .:- "notarisationsContract"
          <*> o .:- "kmdChainSymbol"
          <*> o .:- "kmdNotaryInputs"
          <*> o .:- "consensusTimeout"
    where uninit = error "NotariserConfig not fully initialized"

getConsensusParams :: NotariserConfig -> Zeno EthNotariser ConsensusParams
getConsensusParams NotariserConfig{..} = do
  ident <- asks has
  pure $ ConsensusParams (unMembers members) ident consensusTimeout
