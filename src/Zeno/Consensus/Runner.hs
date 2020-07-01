{-# LANGUAGE RankNTypes #-}

module Zeno.Consensus.Runner where

import Control.Monad.Reader
import Control.Monad.Skeleton
import Control.Monad.State
import Control.Monad.STM (orElse)

import Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.Time.Clock.POSIX

import Network.Ethereum (EthIdent(..), sha3b)

import System.Posix.Signals

import UnliftIO

import Zeno.Consensus.P2P
import Zeno.Consensus.Step
import Zeno.Consensus.Types
import Zeno.Prelude
import Zeno.Process


consensusCap :: CapabilityId
consensusCap = 2

type Runner m = StateT (RunnerState m) m
type RoundsMap m = Map RoundId [Resume m]
type RunnerState m = (RoundsMap m, Map POSIXTime (RunnerAction m), MissCache)
_missCache :: Lens' (RunnerState m) MissCache
_missCache = _3
type Resume m = StepInput -> ConsensusStep m Void
newtype RunnerAction m = RunnerAction (Runner m ())
type MissCache = IntMap.IntMap ((RoundId, Int), RemoteMessage LazyByteString)
type RunnerBase m = (Monad m, HasP2P m, MonadIO m, MonadLogger m)

emptyRunnerState :: RunnerState m
emptyRunnerState = (mempty, mempty, mempty)

startConsensusRunner :: Zeno (Node, PeerState) (ConsensusRunner ZenoRunnerBase)
startConsensusRunner = do
  spawn "Consensus manager" \chan -> do
    registerCapability consensusCap $ send chan . PeerMessage
    registerOnNewPeer $ send chan . NewPeer
    liftIO $ installHandler sigUSR2 (Catch $ send chan DumpStatus) Nothing
    void $ runStateT (forever $ stepConsensusEvent chan) emptyRunnerState


stepConsensusEvent :: Process (ConsensusControlMsg ZenoRunnerBase) -> Runner ZenoRunnerBase ()
stepConsensusEvent recv = do
  delays <- use _2
  case unconsMap delays of
    Nothing -> receiveWait recv >>= handleEvent
    Just ((t, RunnerAction act), rest) -> do
      let doTimeout = _2 .= rest >> act
      now <- liftIO getPOSIXTime
      if now > t
         then doTimeout
         else do
           let micros = Prelude.floor $ (t - now) * 1000000
           delay <- registerDelay micros
           join $
             atomically do
               orElse
                 do receiveSTM recv <&> handleEvent
                 do readTVar delay >>= checkSTM
                    pure doTimeout


handleEvent :: RunnerBase m => ConsensusControlMsg m -> Runner m ()
handleEvent =
  \case
    NewPeer nodeId -> do
      advanceAll $ StepNewPeer nodeId

    GetRoundSize roundId reply -> do
      l <- use $ _1 . at roundId . to (maybe 0 length)
      lift $ reply l

    NewStep roundId skel -> do
      before <- use $ _1 . ix roundId
      let stepId = length before
      let addCachedInputs r = do
            hits <- zoom _missCache $ state $ receiveCacheTake ((== (roundId, stepId)) . fst)
            let inputs = StepData . snd <$> hits
            foldM (\r -> execToWait roundId stepId . r) r inputs
      resume <- execToWait roundId stepId skel >>= addCachedInputs
      _1 . at roundId .= Just (before ++ [resume])

    PeerMessage (RemoteMessage nodeId bs) -> do
      case decodeLazyState bs of
        Right (StepId roundId stepIdVar, msg) -> do
          let rm = RemoteMessage nodeId msg
          let stepId = fromIntegral stepIdVar
          steps <- use $ _1 . ix roundId
          case steps ^? ix stepId of
            Just _ -> do
              advanceOne roundId stepId $ StepData rm
            Nothing -> do
              _missCache %= receiveCachePut ((roundId, stepId), rm)
        Left s -> traceM s -- TODO: handle this

    ReleaseRound    0 roundId ->  _1 . at roundId .= Nothing
    ReleaseRound secs roundId -> do
      t <- liftIO getPOSIXTime <&> (+fromIntegral secs)
      _2 %= Map.insert t (RunnerAction $ _1 . at roundId .= Nothing)

    DumpStatus -> do
      let log = logInfo
      log "Got signal USR2"
      (rounds, delays, rmcache) <- get
      log $ "%i rounds, %i delays, %i cached misses" % (length rounds, length delays, length rmcache)
      forM_ (Map.toList rounds) \(roundId, steps) -> do
        log $ "%s: %i steps" % (show roundId, length steps)


execToWait :: RunnerBase m => RoundId -> Int -> ConsensusStep m Void -> Runner m (Resume m)
execToWait roundId stepId =
  fix \go op -> do
    case debone op of
      Return a                         -> absurd a
      ReceiveFree               :>>= f -> pure f
      GetPeersFree              :>>= f -> lift getPeers >>= go . f
      ConsensusStepLift act     :>>= f -> lift act >>= go . f
      SendRemoteFree nid bs     :>>= f -> go . f =<< lift do
        let msg = encodeLazy (StepId roundId (fromIntegral stepId)) <> bs
        sendRemoteBS nid consensusCap msg
      RegisterTickFree us       :>>= f -> do
        now <- liftIO getPOSIXTime
        let timeout = now + realToFrac us / 1000000
        _2 %= Map.insert timeout (RunnerAction $ advanceOne roundId stepId StepTick)
        go $ f ()

advanceOne :: RunnerBase m => RoundId -> Int -> StepInput -> Runner m ()
advanceOne roundId stepId msg = do
  updateRounds $
    traverseOf (ix roundId . ix stepId) (execToWait roundId stepId . ($ msg))

advanceAll :: RunnerBase m => StepInput -> Runner m ()
advanceAll msg = do
  updateRounds $
    Map.traverseWithKey \roundId steps -> do
      forM (zip [0..] steps) \(stepId, f) -> do
        execToWait roundId stepId $ f msg

updateRounds :: RunnerBase m => (RoundsMap m -> Runner m (RoundsMap m)) -> Runner m ()
updateRounds f = do
  use _1 >>= f >>= assign _1


-- Receive Miss Cache ---------------------------------------------------------

receiveCachePut :: a -> IntMap a -> IntMap a
receiveCachePut miss cache
  | length cache == 0 = IntMap.singleton 0 miss
  | length cache == cacheMaxSize =
      let ((minId, _), nextCache) = IntMap.deleteFindMin cache
       in IntMap.insert (minId + length cache) miss nextCache
  | otherwise =
      let (minId, _) = IntMap.findMin cache
       in IntMap.insert (minId + length cache) miss cache
  where
  cacheMaxSize = 1000

receiveCacheTake :: (a -> Bool) -> IntMap a -> ([a], IntMap a)
receiveCacheTake f = over _1 IntMap.elems . IntMap.partition f
