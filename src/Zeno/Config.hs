{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Zeno.Config where

import           Control.Applicative

import qualified Data.ByteString.Char8 as BS8

import           Network.Ethereum.Crypto
import           Network.Ethereum.Types
import           Network.Simple.TCP

import           Zeno.Data.Aeson hiding (Parser)
import           Zeno.Prelude
import           Zeno.Process.Types

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

data NetworkConfig = NC
  { hostPref :: HostPreference
  , port :: Word16
  } deriving (Show)


data ConsensusNetworkConfig = CNC
  { seeds :: [NodeId]
  , netConf :: NetworkConfig
  } deriving (Show)

consensusDefaultPort :: Word16
consensusDefaultPort = 40440

optBind = option (Host <$> str)
   ( long "bind"
  <> metavar "IP"
  <> value HostIPv4
  <> help "IP to bind to (default: listen on all IPv4 interfaces)" )

optPort = option auto
   ( long "port"
  <> metavar "NUM"
  <> value consensusDefaultPort
  <> showDefault
  <> help "Port to bind to" )

optSeeds = option str
   ( long "seed"
  <> metavar "HOST"
  <> help "ip:port" )

optConsensusConfig = CNC <$> some optSeeds <*> optNetworkConfig
optNetworkConfig = NC <$> optBind <*> optPort

-- Helpers --------------------------------------------------------------------

readJson :: FromJSON a => ReadM a
readJson = eitherReader $ eitherDecode . fromString

optJsonOrStdin :: FromJSON a => Mod ArgumentFields (IO a) -> Parser (IO a)
optJsonOrStdin props =
  let d = eitherDecodeStrict
      f = eitherReader $ \s -> pure <$> d (fromString s)
      f' = either error id . d <$> BS8.getLine
   in argument f $ (value f') <> props
