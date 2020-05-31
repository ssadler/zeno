
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , startSeedNode
  , withConsensusNode
  , runConsensus
  , propose
  , step
  , incStep
  , collectMajority
  , collectThreshold
  , collectMembers
  , majorityThreshold
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Zeno.Process

import Zeno.Consensus.Types
import Zeno.Consensus.Round
import Zeno.Consensus.P2P

import Zeno.Prelude
import Zeno.Console


-- Node -----------------------------------------------------------------------

withConsensusNode :: ConsensusNetworkConfig -> (ConsensusNode -> Zeno () a) -> Zeno () a
withConsensusNode CNC{..} act = do
  withNode netConf \node -> do
    p2p <- withContext (\_ -> node) $ startP2P seeds
    act $ ConsensusNode node p2p


startSeedNode :: NetworkConfig -> IO ()
startSeedNode nc = do
  let cnc = CNC [] nc
  runZeno PlainLog () do
    withConsoleUI do
      withConsensusNode cnc \_ -> threadDelay $ 2^62
