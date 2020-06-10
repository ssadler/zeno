
module Zeno.Consensus.Round
  ( step
  , incStep
  , collectMembers
  , collectMajority
  , collectThreshold
  , collectWith
  , runConsensus
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , majorityThreshold
  ) where

import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Serialize
import           Data.Time.Clock
import           Data.Typeable
import           Unsafe.Coerce

import           Network.Ethereum.Crypto
import           Zeno.Process

import           Zeno.Prelude
import           Zeno.Consensus.P2P
import           Zeno.Consensus.Step
import           Zeno.Consensus.Types
import           Zeno.Console

import           UnliftIO.STM
import           Control.Monad.STM (orElse, throwSTM)
import           UnliftIO.Async (waitCatchSTM, waitSTM)


-- Run round ------------------------------------------------------------------


-- TODO: Get label from thread? Or get rid of threads?
-- Getting rid of the threads for each step would be a good thing. Some of the
-- program state is implicitly encoded in threads, and therefore inaccessible,
-- and more error prone to boot. Rather than threads, there could be a map
-- of open rounds, and a map of steps that are currently executing, with
-- function pointers for them to receive data. The chances are that
-- this could be achieved without changing the structure of the program
-- that much, since it's basically the same thing, you could eliminate alot
-- of locks and queues if there was a main thread doing the work, and a single
-- CPU core should be enough for hundreds of concurrent steps. And you'll get
-- an itrospectable data structure which can be dumped out at runtime, easily
-- paused, resumed etc, without the worry of deadlocks. Could also probably
-- use less mutable variables, which might be a bonus for testing, but not
-- sure about performance.

runConsensus :: (Serialize a, Has ConsensusNode r)
             => String -> ConsensusParams -> a -> Consensus b -> Zeno r b
runConsensus label ccParams@ConsensusParams{..} seed act = do

  ccStepNum <- newTVarIO $ StepNum roundTypeId 0 0
  ccChildren <- newTVarIO []
  let ccSeed = sha3b $ encode (seed, members')
      toCC r = let ccNode = has r in ConsensusContext{..}

  withContext toCC do
    roundId <- getRoundId
    let roundName = "Round %s (%s)" % (roundId, label)
  
    Process{..} <-
      spawn roundName $ \handoff -> do

        withUIProc (UIRound label roundId) do

          handleTimeout roundName do     -- We will re-throw this outside but we
                                         -- don't want it being logged

            act >>= send handoff         -- Send result into handoff so runConsensus can return

            threadDelay $ 10 * 1000000   -- for stragglers to catch up

            pure murphyNoResult          -- This will not get evaluated unless
                                         -- nothing is send to the handoff

    r <- atomically $ orElse (receiveSTM procMbox) (waitSTM procAsync >>= throwSTM)
    logInfo $ "Finished: " ++ roundName
    pure r

  where
  murphyNoResult = murphy "Round finished without a result - this should not happen"
  handleTimeout roundName = do
    handle \c@(ConsensusTimeout _) -> do
      logInfo $ "Timeout: " ++ roundName
      sendUI (UI_Step "Timeout")
      threadDelayS 4
      pure c


-- Round Steps ----------------------------------------------------------------

step :: forall a b. BallotData a => String -> Collect a b -> a -> Consensus b
step name collect obj = do
  incStep $ "collect " ++ name
  -- The step itself is run in a separate thread, and left running even
  -- when it's produced a result
  recv <- spawnStep obj collect

  ConsensusParams{timeout'} <- asks ccParams
  receiveTimeout recv timeout' >>=
    maybe (throwIO $ ConsensusTimeout "") pure

incStep :: String -> Consensus ()
incStep label = do
  major <- incMajorStepNum
  sendUI $ UI_Step $ "%i: %s" % (major, label)


-- Check Majority -------------------------------------------------------------

collectMajority :: Serialize a => Collect a (Inventory a)
collectMajority = collectWith \majority inv -> do
  guard $ length inv >= majority
  pure inv

-- | collectThreshold collects at least n ballots.
--   At least, because it collects the greater of the given n and
--   the majority threshold.
collectThreshold :: Serialize a => Int -> Collect a (Inventory a)
collectThreshold n = collectWith \t inv -> do
  guard $ length inv >= max n t
  pure inv

collectMembers :: Serialize a => [Address] -> Collect a (Inventory a)
collectMembers addrs = collectWith \_ inv -> do
  guard $ all (`Map.member` inv) addrs
  pure inv

collectWith :: Serialize a => (Int -> Inventory a -> Maybe b) -> Collect a b
collectWith f inv = do
  ConsensusParams{..} <- asks has
  let majority = majorityThreshold $ length members'
  pure $ f majority inv


majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
