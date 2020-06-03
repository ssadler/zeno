{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Zeno.Notariser.Stats where

import Data.Serialize
import qualified Data.MessagePack as Msgpack

import qualified Haskoin as H

import Network.Komodo

import Zeno.Data.Aeson
import Zeno.Notariser.Interfaces.KMD
import Zeno.Notariser.Types
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process

import Data.Time.Clock
import Data.Time.Calendar


data ProposerTimeout = ProposerTimeout
  { roundId :: RoundId
  , stepNum :: (Int, Maybe Int)
  , proposer :: Address
  } deriving (Show)

instance ToJSON ProposerTimeout
instance FromJSON ProposerTimeout


recordProposerTimeout :: NotariserConfig -> ProposerTimeout -> Zeno EthNotariser ()
recordProposerTimeout nc@NotariserConfig{..} pt = do
  let label = "proposer timeout: " ++ show (roundId pt)
  t <- liftIO getCurrentTime
  let addr = statsAddress "proposer timeout" $ utctDay t
  let outputs = kmdDataOutputs nc addr (encode pt)
  spawnNoHandle label do
    txid <-
      localZeno filterLogWarn do
        notariseToKMD nc label outputs
    logInfo $ "Posted stats: \"%s\" (%s)" % (label, show txid)

  where
    filterLogWarn app = app { appConsole = FilteredLog LevelWarn (appConsole app) }




statsAddress :: String -> Day -> H.ScriptOutput
statsAddress name day = 
  let s = name ++ show day
      hash = H.ripemd160 (toS s :: ByteString)
   in H.PayPKHash hash
