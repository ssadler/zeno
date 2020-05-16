
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , spawnConsensusNode
  , withConsensusNode
  , runConsensus
  , propose
  , step
  , stepWithTopic
  , collectGeneric
  , collectMajority
  , collectThreshold
  , collectMembers
  , majorityThreshold
  ) where

import Control.Exception.Safe

import Network.NQE
import Network.Transport

import Zeno.Consensus.Types
import Zeno.Consensus.Round
import Zeno.Consensus.P2P as P2P

import Zeno.Config
import Zeno.Prelude


-- Node -----------------------------------------------------------------------

spawnConsensusNode :: ConsensusNetworkConfig -> IO ConsensusNode
spawnConsensusNode CNC{..} = do
  let seeds' = P2P.makeNodeId port <$> seeds
  node <- P2P.startP2P host port seeds'
  pure $ ConsensusNode node


withConsensusNode :: ConsensusNetworkConfig -> (ConsensusNode -> IO a) -> IO a
withConsensusNode conf = do
 bracket (spawnConsensusNode conf)
         (releaseNode . unConsensusNode)

