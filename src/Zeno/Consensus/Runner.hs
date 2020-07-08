{-# LANGUAGE Strict #-}

module Zeno.Consensus.Runner where

import Control.Monad.State

import Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Data.Time.Clock.POSIX
import Data.Tuple (swap)

import Network.Ethereum (EthIdent(..), sha3b)

import UnliftIO

import Zeno.Consensus.P2P
import Zeno.Consensus.Step
import Zeno.Consensus.Types
import Zeno.Prelude
import Zeno.Process
import Zeno.Signal


consensusCap :: CapabilityId
consensusCap = 2

emptyRunnerState :: RunnerState m
emptyRunnerState = (mempty, mempty, mempty)

startConsensusRunner :: ZenoRunnerBase (RunnerProc ZenoRunnerBase)
startConsensusRunner = do
  proc <- newMVar emptyRunnerState
  rio <- askRunInIO
  registerCapability consensusCap $ rio . runBackend proc . onPeerMessage
  registerOnNewPeer $ runBackend proc . advanceAll . StepNewPeer
  installSignalHandler sigUSR1 $ runBackend proc dumpStatus
  spawnNoHandle "Consensus manager" $ runProcessDelays proc
  pure proc

  where
  runProcessDelays proc = do
    forever do
      threadDelay 50000
      now <- liftIO getPOSIXTime
      acts <-
        modifyMVar proc \s -> do
          let (a, b) = Map.spanAntitone (<now) (view _delays s)
          pure (set _delays b s, Map.elems a)
      unless (Prelude.null acts) do
        runBackend proc $ void $ sequence $ unRunnerAction <$> acts

runBackend :: MonadUnliftIO m => MVar s -> StateT s m a -> m a
runBackend proc act = do
  modifyMVar proc $ fmap swap . runStateT act

liftBackend :: Has (ConsensusNode ZenoRunnerBase) r => Runner ZenoRunnerBase a -> Zeno r a
liftBackend act = do
  ConsensusNode{..} <- asks has
  withContext (\_ -> (cpNode, cpPeers)) do
    runBackend cpRunner act

getRoundSize :: RunnerBase m => RoundId -> StateT (RunnerState m) m Int
getRoundSize roundId = do
  l <- use $ _1
  let f s = fromEnum $ stRoundId s == roundId
  pure $ Map.foldrWithKey (\s _ n -> n + f s) 0 l

onPeerMessage :: RunnerBase m => RemoteMessage LazyByteString -> Runner m ()
onPeerMessage (RemoteMessage nodeId bs) = do
  case decodeLazyState bs of
    Right (stepId, msg) -> do
      let rm = RemoteMessage nodeId msg
      
      mstep <- use $ _1 . at stepId
      case mstep of
        Just _ -> do
          advanceOne stepId $ StepData rm
        Nothing -> do
          _missCache %= receiveCachePut (stepId, rm)

    Left s -> traceM s -- TODO: handle this

releaseRound :: RunnerBase m => Int -> RoundId -> Runner m ()
releaseRound secs roundId = do
  let f = _1 %= Map.filterWithKey (\s _ -> stRoundId s /= roundId)
  if secs == 0
     then f
     else do
       t <- liftIO getPOSIXTime <&> (+fromIntegral secs)
       _2 %= Map.insert t (RunnerAction f)

newStep :: RunnerBase m => StepId -> ConsensusStep m Void -> Runner m ()
newStep stepId skel = do
  resume <- execToWait stepId skel >>= addCachedInputs
  _1 . at stepId .= Just resume
  where
  addCachedInputs r = do
    hits <- zoom _missCache do
      state $ receiveCacheTake \(k, _) -> k == stepId
    let inputs = StepData . snd <$> hits
    foldM (\r -> execToWait stepId . r) r inputs

dumpStatus :: RunnerBase m => Runner m ()
dumpStatus = do
  let log = logInfo
  log "Got signal USR2"
  (steps, delays, rmcache) <- get
  log $ "%i steps, %i delays, %i cached misses" % (length steps, length delays, length rmcache)
  forM_ (Map.toList steps) \(stepId, _) -> do
    log $ show stepId

execToWait :: RunnerBase m => StepId -> ConsensusStep m Void -> Runner m (Resume m)
execToWait stepId =
  fix \go op -> do
    case debone op of
      Return a                         -> absurd a
      ReceiveFree               :>>= f -> pure f
      GetPeersFree              :>>= f -> lift getPeers >>= go . f
      ConsensusStepLift act     :>>= f -> lift act >>= go . f
      SendRemoteFree nid bs     :>>= f -> do
        go . f =<< lift do
          let msg = encodeLazy stepId <> bs
          sendRemoteBS nid consensusCap msg
      RegisterTickFree tick us  :>>= f -> do
        now <- liftIO getPOSIXTime
        let timeout = now + realToFrac us / 1000000
        _2 %= Map.insert timeout (RunnerAction $ advanceOne stepId $ StepTick tick)
        go $ f ()

advanceOne :: RunnerBase m => StepId -> StepInput -> Runner m ()
advanceOne stepId msg = do
  updateRounds $
    traverseOf (ix stepId) (execToWait stepId . ($ msg))

advanceAll :: RunnerBase m => StepInput -> Runner m ()
advanceAll msg = do
  updateRounds $
    Map.traverseWithKey \stepId f ->
      execToWait stepId $ f msg

updateRounds :: RunnerBase m => (RoundsMap m -> Runner m (RoundsMap m)) -> Runner m ()
updateRounds f = do
  use _1 >>= f >>= assign _1


-- Receive Miss Cache ---------------------------------------------------------

receiveCachePut :: a -> IntMap a -> IntMap a
receiveCachePut miss cache =
  case IntMap.lookupMax cache of
    Nothing -> IntMap.singleton 0 miss
    Just (maxId, _) ->
      let next = IntMap.insert (maxId + 1) miss cache
       in if length next == 1001 then IntMap.deleteMin next else next

receiveCacheTake :: (a -> Bool) -> IntMap a -> ([a], IntMap a)
receiveCacheTake f m =
  let (hits, next) = IntMap.partition f m
      -- newer items have a higher id. in the case there there are matches,
      -- prune removes all items that are i-50 or lower, so stale items will
      -- continually get cleared out.
      prune = case IntMap.lookupMin hits of
                Nothing -> id
                Just (i, _) -> snd . IntMap.split (i-50)
   in (IntMap.elems hits, prune next)
