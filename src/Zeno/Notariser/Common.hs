
module Zeno.Notariser.Common
  ( module Zeno.Notariser.Types
  , getConsensusParams
  ) where

import Zeno.Notariser.Types
import Zeno.Consensus
import Zeno.Prelude


getConsensusParams :: NotariserConfig -> RoundType -> Zeno EthNotariser ConsensusParams
getConsensusParams nc@NotariserConfig{..} roundType = do
  ident <- asks has
  let members' = members
      ident' = ident
      timeout' = consensusTimeout
      roundTypeId = fromIntegral $ fromEnum roundType
  pure $ ConsensusParams{..}

