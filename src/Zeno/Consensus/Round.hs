
module Zeno.Consensus.Round where

import           Control.Monad.Reader

import           Data.Bits
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
    handle \ConsensusTimeout -> do
      logInfo $ "Timeout: " ++ roundName
      sendUI (UI_Step "Timeout")
      threadDelayS 4
      pure ConsensusTimeout


-- Round Steps ----------------------------------------------------------------

step :: forall a b. BallotData a => String -> a -> Collect a b -> Consensus b
step name obj collect = do
  stepOptData name (Just obj) collect

stepOptData :: BallotData a => String -> Maybe a -> Collect a b -> Consensus b
stepOptData name mobj collect = do
  incStep $ "collect " ++ name
  step' name mobj collect

step' :: forall a b. BallotData a => String -> Maybe a -> Collect a b -> Consensus b
step' name mobj collect = do
  -- The step itself is run in a separate thread, and left running even
  -- when it's produced a result
  recv <- spawnStep mobj
  collect recv <*
    spawnNoHandle ("eater for: " ++ name) do   -- So that the step doesnt get blocked
      forever $ receiveWait recv

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

collectMember :: Serialize a => Address -> Collect a (Ballot a)
collectMember addr = collectWith \_ inv ->
  Map.lookup addr inv <&> uncurry (Ballot addr)

collectWith :: Serialize a => (Int -> Inventory a -> Maybe b) -> Collect a b
collectWith f recv = do
  ConsensusParams{..} <- asks has
  let majority = majorityThreshold $ length members'

  either pure (\() -> throwIO ConsensusTimeout) =<<
    runExceptT do
      receiveDuring recv timeout' $
         maybe (pure ()) throwError . f majority



majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
