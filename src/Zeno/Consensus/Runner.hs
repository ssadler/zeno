
module Zeno.Consensus.Runner where

import Control.Monad.Reader
import Control.Monad.Skeleton
import Control.Monad.State
import Control.Monad.STM (orElse)

import Data.Serialize (encode, decode)
import qualified Data.Map as Map
import Data.Time.Clock.POSIX

import Lens.Micro.Platform ((.=))

import Network.Ethereum (EthIdent(..), sha3b)

import UnliftIO

import Zeno.Consensus.P2P
import Zeno.Consensus.Step
import Zeno.Consensus.Types
import Zeno.Prelude
import Zeno.Process


consensusCapId :: CapabilityId
consensusCapId = 2

type Runner = StateT RunnerState ConsensusRunnerBase
type RoundsMap = Map RoundId [Resume]
type RunnerState = (RoundsMap, Map POSIXTime RunnerAction)
type Resume = StepInput -> ConsensusStep ConsensusRunnerBase Void
newtype RunnerAction = RunnerAction (Runner ())

startConsensusRunner :: Zeno (Node, PeerState) ConsensusRunner
startConsensusRunner = do
  spawn "Consensus manager" \chan -> do
    registerCapability 2 $ send chan . PeerMessage
    registerOnNewPeer $ send chan . NewPeer
    void $ runStateT (forever $ stepConsensusEvent chan) (mempty, mempty)

stepConsensusEvent :: Process ConsensusControlMsg -> Runner ()
stepConsensusEvent recv = do
  (_, delays) <- get
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


handleEvent :: ConsensusControlMsg -> Runner ()
handleEvent =
  \case
    NewPeer nodeId -> do
      advanceAll $ StepNewPeer nodeId

    GetRoundSize roundId reply -> do
      l <- use $ _1 . at roundId . to (maybe 0 length)
      putMVar reply l

    NewStep roundId skel -> do
      before <- use $ _1 . ix roundId
      resume <- execToWait roundId (length before) skel
      _1 . ix roundId .= (before ++ [resume])

    PeerMessage (RemoteMessage nodeId bs) -> do
      case decode bs of
        Right (roundId, stepId, msg) ->
          advanceOne roundId stepId $ StepData $ RemoteMessage nodeId msg
        Left _ -> pure ()  -- TODO: handle this

    ReleaseRound roundId -> do
      t <- liftIO getPOSIXTime <&> (+60)
      _2 %= Map.insert t (RunnerAction $ _1 . at roundId .= Nothing)


execToWait :: RoundId -> Int -> ConsensusStep ConsensusRunnerBase Void -> Runner Resume
execToWait roundId stepId =
  fix \go op -> do
    case debone op of
      Return a                         -> absurd a
      ReceiveFree               :>>= f -> pure f
      GetPeersFree              :>>= f -> lift getPeers >>= go . f
      SendRemoteFree nid a      :>>= f -> lift (sendRemote nid consensusCapId a) >>= go . f
      ConsensusStepLift act     :>>= f -> lift act >>= go . f
      RegisterTickFree us       :>>= f -> do
        now <- liftIO getPOSIXTime
        let timeout = now + realToFrac us / 1000000
        _2 %= Map.insert timeout (RunnerAction $ advanceOne roundId stepId StepTick)
        go $ f ()

advanceOne :: RoundId -> Int -> StepInput -> Runner ()
advanceOne roundId stepId msg = do
  updateRounds $
    traverseOf (ix roundId . ix stepId) (execToWait roundId stepId . ($ msg))
  
advanceAll :: StepInput -> Runner ()
advanceAll msg = do
  updateRounds $
    Map.traverseWithKey \roundId steps -> do
      forM (zip [0..] steps) \(stepId, f) -> do
        execToWait roundId stepId $ f msg

updateRounds :: (RoundsMap -> Runner RoundsMap) -> Runner ()
updateRounds f = do
  use _1 >>= f >>= assign _1
