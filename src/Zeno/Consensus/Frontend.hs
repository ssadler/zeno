
module Zeno.Consensus.Frontend where

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


runConsensus :: (Has ZenoConsensusNode r, Serialize a)
             => String -> ConsensusParams -> a -> Consensus (Zeno r) o -> Zeno r o
runConsensus label params@ConsensusParams{..} seedData act = do
  let seed = sha3b $ encode (members', seedData)
  manager <- asks $ cpRunner . has
  let roundId10 = toFixedR $ fromFixed seed :: Bytes10
  let roundId = roundId10 `bappend` newFixed roundTypeId

  roundSize <- getRoundSize manager roundId
  when (roundSize /= 0) do
    throwIO ConsensusTopicRegistered

  let roundName = "Round %s (%s)" % (roundId, label)
  logInfo $ "Starting: " ++ roundName
  sendUI $ UI_Process $ Just $ UIRound label roundId
  round <- RoundData manager params seed roundId <$> newIORef 0 <*> newIORef 0
  onException
    do runReaderT act round <*
         send manager (ReleaseRound 60 roundId)
    do send manager (ReleaseRound 0 roundId)

getRoundSize :: (MonadIO m1, MonadIO m2) => Process (ConsensusControlMsg m2) -> RoundId -> m1 Int
getRoundSize manager roundId = do
  m <- newEmptyMVar
  send manager $ GetRoundSize roundId $ putMVar m
  takeMVar m


-- Round Steps ----------------------------------------------------------------

step :: BallotData a => String -> a -> Collect a (Zeno r) b -> Consensus (Zeno r) b
step name obj collect = do
  stepOptData name (Just obj) collect

stepOptData :: BallotData i => String -> Maybe i -> Collect i (Zeno r) o -> Consensus (Zeno r) o
stepOptData label i collect = do
  r@RoundData{..} <- ask
  let ident = has params
  roundSize <- atomicModifyIORef mutStepNum \i -> (i+1, i)
  let stepName = "%i: %s" % (roundSize+1, label)
  logDebug $ "Step " ++ stepName
  lift $ sendUI $ UI_Step stepName

  let ConsensusParams{..} = params
  let stepId = StepId roundId roundSize 0
  invRef <- newIORef (0, mempty :: Inventory i)

  recv <- newEmptyTMVarIO
  wrapper <- newMVar recv
  let
    yield inv = do
      tryReadMVar wrapper >>=
        \case
          Nothing -> pure ()
          Just recv -> atomically $ putTMVar recv inv

  let step = Step invRef members' (Set.fromList members') stepId ident yield
  send manager $ NewStep stepId $ createStep step i
  withException
    do collect recv <* takeMVar wrapper
    \ConsensusTimeout -> logInfo "Timeout"


-- | Increment step retry on timeout
withRetry :: Int -> Consensus (Zeno r) o -> Consensus (Zeno r) o
withRetry n act = do
  r@RoundData{..} <- ask
  stepNum <- readIORef mutStepNum
  finally
    do
      fix1 n \go left -> do
        catch
          do act
          \ConsensusTimeout -> do
            if left == 0
               then throwIO ConsensusTimeout
               else do
                 logInfo "Retry step"
                 writeIORef mutStepNum stepNum
                 modifyIORef mutStepRetry (+1)
                 go (left-1)

    do writeIORef mutStepRetry 0


-- Check Majority -------------------------------------------------------------

type Collect i m o = TMVar (Inventory i) -> Consensus m o

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
        lift (f majority inv) >>= maybe (pure ()) throwError


majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
