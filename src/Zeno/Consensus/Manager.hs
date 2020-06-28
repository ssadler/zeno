
module Zeno.Consensus.Manager where

import Control.Monad.Reader

import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Map as Map

import Network.Ethereum (sha3b)

import UnliftIO

import Zeno.Consensus.Free
import Zeno.Consensus.Step
import Zeno.Consensus.Types
import Zeno.Prelude
import Zeno.Process


data Round r m = Round
  { manager :: ConsensusManager m
  , seed    :: Bytes32
  , params  :: ConsensusParams
  , steps   :: [ConsensusStep m ()]
  , ctx     :: r
  }

getRoundId2 :: Round r m -> RoundId
getRoundId2 = toFixedR . fromFixed . seed


data ConsensusControlMsg m
  = NewStep RoundId (ConsensusStep m ())
  | GetRoundSize RoundId (MVar Int)

type ConsensusManager m = Process (ConsensusControlMsg m)

type CollectIt r i m o = TMVar (Inventory i) -> m o

startConsensusManager :: Zeno Node (ConsensusManager (Zeno r))
startConsensusManager = do
  spawn "Consensus manager" \recv -> do
    fix1 emptyRounds \go rounds -> do
      receiveWait recv >>=
        \case
          GetRoundSize roundId reply -> do
            putMVar reply $ length $ rounds ^? at roundId . to (maybe 0 length)
          NewStep roundId skel -> do
            go $ undefined
  where
  emptyRounds :: Map RoundId [ConsensusStep m ()]
  emptyRounds = mempty

runRound :: forall a m r. Has (ConsensusManager (Zeno r)) r
         => ConsensusParams -> Bytes32 -> Zeno (Round r (Zeno r)) a -> Zeno r a
runRound params seed act = do
  manager <- asks has
  let round b = Round manager seed params mempty b
  withContext round act

runStep :: (BallotData i, MonadLogger m, MonadIO m, MonadReader (Round r m) m)
        => Maybe i -> CollectIt r i m o -> m o
runStep i collect = do
  r@Round{..} <- ask
  let roundId = getRoundId2 r
  roundSize <- request manager (GetRoundSize roundId)

  let ConsensusParams{..} = params
  let stepNum = StepNum roundTypeId (fromIntegral roundSize) 0
  invRef <- newIORef (0, mempty :: Inventory i)
  let phash = sha3b $ encode (seed, stepNum)
  let pid = ProcessId . bappend roundId . reFixed <$> getStepSeed

  recv <- newEmptyTMVarIO
  wrapper <- newMVar recv
  let
    yield inv = do
      tryReadMVar wrapper >>=
        \case
          Nothing -> pure ()
          Just recv -> atomically $ putTMVar recv inv

  let step = Step undefined invRef members' (Set.fromList members') stepNum yield
  send manager $ NewStep roundId $ createStep params step i
  collect recv <* takeMVar wrapper
