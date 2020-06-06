
module Zeno.Consensus.Round
  ( step
  , step'
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
runConsensus label ccParams@ConsensusParams{..} entropy act = do

  ccStepNum <- newTVarIO $ StepNum roundTypeId 0 0
  ccChildren <- newTVarIO []
  let ccEntropy = sha3b $ encode entropy
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


-- Coordinate Round -----------------------------------------------------------

-- | Step initiates the procedure of exchanging signed messages.
-- | The step is run in a separate thread, and this thread will wait
-- | until the collect condition has been fulfilled.
step :: BallotData a => String -> Collect a b -> a -> Consensus b
step name collect obj = do
  incStep $ "collect " ++ name
  step' name collect obj

step' :: forall a b. BallotData a => String -> Collect a b -> a -> Consensus b
step' name collect obj = do
  ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
  let sig = sign sk message
      ballot = Ballot myAddr sig obj
      errTimeout = ConsensusTimeout ("Timeout after %i seconds" % quot timeout' 1000000)

  recv <- spawnStep ballot

  -- The flow is inverted here, we use Left to jump out early
  r <- runExceptT do
    receiveDuring recv timeout' \inv -> do
      pass <- lift $ collect inv
      maybe (pure ()) throwError pass

  case r of
    Left inv -> pure inv
    Right () -> throwIO errTimeout 
  where

  -- This thing is a bit terrible. It would be easy to make a mistake using it.
  message =
    case cast obj of
      Just b -> b
      Nothing -> sha3b $ encode obj

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
