
module Zeno.Consensus.Round where

import           Control.Monad.Reader

import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import           Zeno.Consensus.Runner
import           Zeno.Console

import           UnliftIO

import           UnliftIO.STM
import           Control.Monad.STM (orElse, throwSTM)
import           UnliftIO.Async (waitCatchSTM, waitSTM)


runConsensus :: forall a o m r.  (Has ConsensusNode r, Serialize a)
             => String -> ConsensusParams -> a -> Consensus (Zeno r) o -> Zeno r o
runConsensus label params@ConsensusParams{..} seedData act = do
  let seed = sha3b $ encode (members', seedData)
  manager <- asks $ cpRunner . has
  let roundId = toFixedR $ fromFixed seed
  let roundName = "Round %s (%s)" % (roundId, label)
  logInfo $ "Starting: " ++ roundName
  sendUI $ UI_Process $ Just $ UIRound label roundId
  let round = RoundData manager seed params mempty
  finally
    do runReaderT act round
    do send manager $ ReleaseRound roundId


-- Round Steps ----------------------------------------------------------------

step :: BallotData a => String -> a -> Collect a (Zeno r) b -> Consensus (Zeno r) b
step name obj collect = do
  stepOptData name (Just obj) collect

stepOptData :: (BallotData i, MonadLoggerUI m)
        => String -> Maybe i -> Collect i m o -> Consensus m o
stepOptData label i collect = do
  r@RoundData{..} <- ask
  let ident = has params
  let roundId = toFixedR $ fromFixed seed
  roundSize <- request manager (GetRoundSize roundId)
  lift $ sendUI $ UI_Step $ "%i: %s" % (roundSize+1, label)

  let ConsensusParams{..} = params
  let stepNum = StepNum roundTypeId (fromIntegral roundSize) 0
  invRef <- newIORef (0, mempty :: Inventory i)
  let phash = sha3b $ encode (seed, stepNum)

  recv <- newEmptyTMVarIO
  wrapper <- newMVar recv
  let
    yield inv = do
      tryReadMVar wrapper >>=
        \case
          Nothing -> pure ()
          Just recv -> atomically $ putTMVar recv inv

  let step = Step invRef members' (Set.fromList members') roundId stepNum ident yield
  send manager $ NewStep roundId $ createStep params step i
  collect recv <* takeMVar wrapper


-- Check Majority -------------------------------------------------------------

type Base m = (MonadLoggerUI m)

collectMajority :: (Base m, Serialize a) => Collect a m (Inventory a)
collectMajority = collectWith \t inv -> do
  let l = length inv
  lift $ sendUI $ UI_MofN l t
  pure $ if l >= t then Just inv else Nothing

-- | collectThreshold collects at least n ballots.
--   At least, because it collects the greater of the given n and
--   the majority threshold.
collectThreshold :: (Base m, Serialize a) => Int -> Collect a m (Inventory a)
collectThreshold threshold = collectWith \majority inv -> do
  let t = max threshold majority
  let l = length inv
  -- sendUI $ UI_MofN l t
  pure $ if l >= t then Just inv else Nothing

collectMembers :: (Base m, Serialize a) => [Address] -> Collect a m [Ballot a]
collectMembers addrs = collectWith \_ inv -> do
  let n = length addrs
      r = catMaybes [Map.lookup a inv | a <- addrs]
      m = length r
  -- sendUI $ UI_MofN m n
  pure $
    if m == n
       then Just [Ballot a s o | (a, (s, o)) <- zip addrs r]
       else Nothing

collectMember :: (Base m, Serialize a) => Address -> Collect a m (Ballot a)
collectMember addr = fmap head . collectMembers [addr]

collectWith :: (Base m, Serialize a) => (Int -> Inventory a -> Consensus m (Maybe b)) -> Collect a m b
collectWith f recv = do
  ConsensusParams{..} <- asks has
  let majority = majorityThreshold $ length members'

  either pure (\() -> throwIO ConsensusTimeout) =<<
    runExceptT do
      receiveDuring recv timeout' $ \inv ->
         maybe (pure ()) throwError =<< lift (f majority inv)


majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
