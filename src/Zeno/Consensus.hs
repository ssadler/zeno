
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , Address
  , collectMajority
  , collectMember
  , collectMembers
  , collectThreshold
  , collectWith
  , majorityThreshold
  , runConsensus
  , startSeedNode
  , step
  , stepOptData
  , withConsensusNode
  , withConsensusRunnerContext
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

withConsensusNode :: ConsensusNetworkConfig -> Zeno (ConsensusNode ZenoRunnerBase) a -> Zeno () a
withConsensusNode netconf act = do
  withConsensusRunnerContext netconf do
    runner <- startConsensusRunner
    withContext (\(node, p2p) -> ConsensusNode node p2p runner) act

withConsensusRunnerContext :: ConsensusNetworkConfig -> Zeno (Node, PeerState) a -> Zeno () a
withConsensusRunnerContext CNC{..} act = do
  withNode netConf do
    p2p <- startP2P seeds
    node <- ask
    let runnerCtx = (node, p2p)
    withContext (const runnerCtx) act


startSeedNode :: NetworkConfig -> ConsoleArgs -> IO ()
startSeedNode nc consoleArgs = do
  let cnc = CNC [] nc
  runZeno defaultLog () do
    withConsole consoleArgs LevelDebug do
      withConsensusRunnerContext cnc $ threadDelay $ 2^62
