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
type RoundsMap m = Map StepId (Resume m)
type RunnerState m = (RoundsMap m, Map POSIXTime (RunnerAction m), MissCache)
_missCache :: Lens' (RunnerState m) MissCache
_missCache = _3
type Resume m = StepInput -> ConsensusStep m Void
newtype RunnerAction m = RunnerAction (Runner m ())
type MissCache = IntMap.IntMap (StepId, RemoteMessage LazyByteString)
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
      l <- use $ _1
      let f s = fromEnum $ stRoundId s == roundId
      let len = Map.foldrWithKey (\s _ n -> n + f s) 0 l
      lift $ reply len

    NewStep stepId skel -> do
      let addCachedInputs r = do
            hits <- zoom _missCache do
              state $ receiveCacheTake \(k, _) -> k == stepId
            let inputs = StepData . snd <$> hits
            foldM (\r -> execToWait stepId. r) r inputs
      resume <- execToWait stepId skel >>= addCachedInputs
      _1 . at stepId .= Just resume

    PeerMessage (RemoteMessage nodeId bs) -> do
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

    ReleaseRound secs roundId -> do
      let f = _1 %= Map.filterWithKey (\s _ -> stRoundId s /= roundId)
      if secs == 0
         then f
         else do
           t <- liftIO getPOSIXTime <&> (+fromIntegral secs)
           _2 %= Map.insert t (RunnerAction f)

    DumpStatus -> do
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
      RegisterTickFree us       :>>= f -> do
        now <- liftIO getPOSIXTime
        let timeout = now + realToFrac us / 1000000
        _2 %= Map.insert timeout (RunnerAction $ advanceOne stepId StepTick)
        go $ f ()

advanceOne :: RunnerBase m => StepId -> StepInput -> Runner m ()
advanceOne stepId msg = do
  updateRounds $
    traverseOf (ix stepId) (execToWait stepId . ($ msg))

advanceAll :: RunnerBase m => StepInput -> Runner m ()
advanceAll msg = do
  updateRounds $
    Map.traverseWithKey \stepId f -> do
      execToWait stepId $ f msg

updateRounds :: RunnerBase m => (RoundsMap m -> Runner m (RoundsMap m)) -> Runner m ()
updateRounds f = do
  use _1 >>= f >>= assign _1


-- Receive Miss Cache ---------------------------------------------------------

receiveCachePut :: a -> IntMap a -> IntMap a
receiveCachePut miss cache
  | length cache == 0 = IntMap.singleton 0 miss
  | otherwise =
      let (maxId, _) = IntMap.findMax cache
       in IntMap.insert (maxId + 1) miss cache

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
