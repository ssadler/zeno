{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Zeno.Config where

import           Control.Applicative

import qualified Data.ByteString.Char8 as BS8

import           Network.Ethereum.Crypto
import           Network.Ethereum.Types

import           Zeno.Data.Aeson hiding (Parser)
import           Zeno.Prelude

import           Options.Applicative


optGethConfig :: Parser GethConfig
optGethConfig = GethConfig <$>
  strOption
     ( long "geth"
    <> value "http://localhost:8545"
    <> help "Geth endpoint"
    <> metavar "URL"
    <> showDefault )

optKmdConfigPath :: Parser String
optKmdConfigPath = strOption
   ( long "kmd"
  <> value "~/.komodo/komodo.conf"
  <> help "Path to komodo.conf"
  <> metavar "KMDCONF"
  <> showDefault )

optGateway :: Parser Address
optGateway =
  option auto
     ( long "gateway"
    <> metavar "ADDRESS"
    <> help "Gateway contract address 0x..." )

data ConsensusNetworkConfig = CNC
  { host :: String
  , port :: Word16
  , seeds :: [String]
  }

consensusDefaultPort :: Word16
consensusDefaultPort = 40440

optHost = strOption
   ( long "host"
  <> metavar "IP"
  <> help "Public IP to bind to" )

optPort = option auto
   ( long "port"
  <> metavar "NUM"
  <> value consensusDefaultPort
  <> showDefault
  <> help "Port to bind to" )

optSeeds = strOption
   ( long "seed"
  <> metavar "HOST"
  <> help "ip[:port]" )

optConsensusConfig = CNC <$> optHost <*> optPort <*> some optSeeds

-- Helpers --------------------------------------------------------------------

readJson :: FromJSON a => ReadM a
readJson = eitherReader $ eitherDecode . fromString

optJsonOrStdin :: FromJSON a => Mod ArgumentFields (IO a) -> Parser (IO a)
optJsonOrStdin props =
  let d = eitherDecodeStrict
      f = eitherReader $ \s -> pure <$> d (fromString s)
      f' = either error id . d <$> BS8.getLine
   in argument f $ (value f') <> props
