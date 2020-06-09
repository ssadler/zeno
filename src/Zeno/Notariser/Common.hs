
module Zeno.Notariser.Common
  ( module Zeno.Notariser.Types
  , getConsensusParams
  ) where

import Data.Serialize

import qualified Haskoin as H

import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling

import Zeno.Data.Aeson
import Zeno.Notariser.Types
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process

import Data.Time.Clock
import Data.Time.Calendar


getConsensusParams :: NotariserConfig -> RoundType -> Zeno EthNotariser ConsensusParams
getConsensusParams nc@NotariserConfig{..} roundType = do
  ident <- asks has
  let members' = members
      ident' = ident
      timeout' = consensusTimeout
      onProposerTimeout' = Nothing
      roundTypeId = fromIntegral $ fromEnum roundType
  pure $ ConsensusParams{..}

