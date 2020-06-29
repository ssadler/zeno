
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , Address
  , startSeedNode
  , withConsensusNode
  , runConsensus
  , step
  , stepOptData
  , collectMajority
  , collectThreshold
  , collectMember
  , collectMembers
  , collectWith
  , majorityThreshold
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Network.Ethereum.Crypto (Address)

import Zeno.Consensus.P2P
import Zeno.Consensus.Round
import Zeno.Consensus.Runner
import Zeno.Consensus.Types

import Zeno.Process
import Zeno.Prelude
import Zeno.Console


-- Node -----------------------------------------------------------------------

withConsensusNode :: ConsensusNetworkConfig -> Zeno ConsensusNode a -> Zeno () a
withConsensusNode CNC{..} act = do
  withNode netConf do
    p2p <- startP2P seeds
    node <- ask
    let runnerCtx = (node, p2p)
    runner <- withContext (const runnerCtx) startConsensusRunner
    let ctx = ConsensusNode node p2p runner
    withContext (const ctx) act


startSeedNode :: NetworkConfig -> ConsoleArgs -> IO ()
startSeedNode nc consoleArgs = do
  let cnc = CNC [] nc
  runZeno defaultLog () do
    withConsole consoleArgs LevelDebug do
      withConsensusNode cnc $ threadDelay $ 2^62
