{-# LANGUAGE PartialTypeSignatures #-}

module Zeno.Consensus.Step where

-- This module deals with exchanging messages and building inventory.
-- The entry point is `runStep`. Each step has a topic, and each member
-- will provide an input. Messages are exhanged until all participants
-- have a full inventory or the step is cancelled.

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.STM (orElse)
import           Control.Monad.Skeleton

import           Data.Bits
import           Data.Serialize
import qualified Data.Map.Strict as Map
import           Data.Map.Merge.Strict
import qualified Data.Set as Set
import           Data.Typeable

import           GHC.Generics (Generic)

import           Network.Ethereum.Crypto

import           Zeno.Process
import           Zeno.Consensus.P2P
import           Zeno.Consensus.Free
import           Zeno.Consensus.Types
import           Zeno.Console
import           Zeno.Prelude

import           UnliftIO
import           UnliftIO.Concurrent


inventoryQueryInterval :: Int
inventoryQueryInterval = 200 * 1000

data Step i = Step
  { processId  :: ProcessId
  , ioInv      :: IORef (PackedInteger, Inventory i)
  , members    :: [Address]
  , membersSet :: Set Address
  , stepNum    :: StepNum
  }


makeStepContext :: Consensus (Step i)
makeStepContext = do
  ConsensusContext{ccParams = ConsensusParams{..},..} <- ask
  roundId <- getRoundId
  processId <- ProcessId . bappend roundId . reFixed <$> getStepSeed
  ioInv <- newIORef (0, mempty)
  stepNum <- getStepNum
  pure $ Step processId ioInv members' (Set.fromList members') stepNum


spawnStep :: BallotData i => Maybe i -> Consensus (Process (Inventory i))
spawnStep mobj = do
  step@Step{..} <- makeStepContext
  let stepName = "step: " ++ show processId
  mballot <- forM mobj \obj -> do
    ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
    let sighash = getBallotSighash obj
    mySig <- signIO sk sighash
    pure $ Ballot myAddr mySig obj
  spawn stepName \process -> do
    runStepFree step (send process) $ runStep step mballot


runStepFree :: forall i a. BallotData i
            => Step i -> (Inventory i -> Consensus ())
            -> ConsensusStep i Consensus a -> Consensus a
runStepFree step@Step{..} yield =
  go (Nothing :: Maybe (RemoteReceiver (WrappedStepMessage i)))
  where
  go mrecv op =
    case debone op of
      Return a                         -> pure a
      ConsensusStepLift act     :>>= f -> act >>= go mrecv . f
      GetIdentFree              :>>= f -> asks has >>= go mrecv . f
      GetPeersFree              :>>= f -> getPeers >>= go mrecv . f
      SendRemoteFree nid pid a  :>>= f -> sendRemote nid pid a >>= go mrecv . f
      YieldFree inv             :>>= f -> yield inv >>= go mrecv . f

      RegisterOnNewPeerFree act :>>= f -> do
        registerOnNewPeer $ runStepFree step yield . act
        go mrecv $ f ()

      ReceiveFree               :>>= f -> do
        recv <- maybe (subscribe processId) pure mrecv
        delay <- registerDelay inventoryQueryInterval
        go (Just recv) . f =<<
          atomically do
            orElse
              do receiveSTM recv >>= pure . Just
              do readTVar delay >>= checkSTM
                 pure Nothing


type Base m = (MonadLogger m, MonadIO m)


runStep :: (BallotData i, Base m) => Step i -> Maybe (Ballot i) -> ConsensusStep i m ()
runStep step@Step{..} mballot = do
  bone $ RegisterOnNewPeerFree $ onNewPeer step
  forM_ mballot \(Ballot myAddr sig obj) -> do
    onInventoryData step True $ Map.singleton myAddr (sig, obj)
  buildInventoryLoop step



buildInventoryLoop :: Base m => BallotData i => Step i -> ConsensusStep i m ()
buildInventoryLoop step@Step{..} = do
  forever do
    fix1 mempty $ \go !idxs -> do
      bone ReceiveFree >>=
        maybe
          do sendInventoryQueries step $ Map.toList idxs
          do authenticate step
               \peer (StepMessage theirIdx theirReq theirData) -> do
                 onInventoryData step False theirData
                 onInventoryRequest step peer theirReq
                 go $ Map.insert peer theirIdx idxs



onNewPeer :: (BallotData i, Base m) => Step i -> NodeId -> ConsensusStep i m ()
onNewPeer Step{..} peer = do
  (idx, inv) <- readIORef ioInv
  let msg = StepMessage idx 0 mempty
  sendAuthenticated Step{..} [peer] msg

onInventoryRequest :: Base m => BallotData i => Step i -> NodeId -> PackedInteger -> ConsensusStep i m ()
onInventoryRequest _ _ 0 = pure ()
onInventoryRequest step@Step{..} peer theirReq = do
  (myIdx, inv) <- readIORef ioInv
  let subset = getInventorySubset theirReq members inv
  sendAuthenticated step [peer] $ StepMessage myIdx 0 subset


onInventoryData :: (BallotData i, Base m) => Step i -> Bool -> Inventory i -> ConsensusStep i m ()
onInventoryData _ _ theirInv | Map.null theirInv = pure ()
onInventoryData step@Step{..} forwardInv theirInv = do

  let theirIdx = inventoryIndex members theirInv
  (oldIdx, oldInv) <- readIORef ioInv

  unless (0 == theirIdx .&. complement oldIdx) do

    case mergeInventory membersSet oldInv theirInv of
      Left s -> logWarn s

      Right newInv -> do
        let newIdx = theirIdx .|. oldIdx
        writeIORef ioInv (newIdx, newInv)

        bone $ YieldFree newInv
        peers <- bone GetPeersFree
        let fwdInv = if forwardInv then newInv else mempty
        sendAuthenticated step peers $ StepMessage newIdx 0 fwdInv


mergeInventory :: BallotData i => Set Address -> Inventory i -> Inventory i -> Either String (Inventory i)
mergeInventory membersSet ours theirs = do
  mergeA preserveMissing
         (traverseMissing validateNew)
         (zipWithAMatched validateExisting)
         ours theirs
  where
  validateNew addr (sig, val) = do
    let sighash = getBallotSighash val
    when (Set.notMember addr membersSet) do
      throwError "Got ballot data for non member"
    let r = unsafePerformIO $ recoverAddr sighash sig
    when (r /= addr) do
      throwError "Got ballot data with invalid signature"
    pure (sig, val)
    
  -- We shouldn't be receiving data that we already have for a key, ever, ideally.
  -- If anything, maybe we should flag the sender.
  -- Currently, this will no be called since we compare the calculated indexes
  -- before called mergeInventory
  validateExisting _ ours _ = pure ours


sendInventoryQueries :: (BallotData i, Base m) => Step i -> [(NodeId, PackedInteger)] -> ConsensusStep i m ()
sendInventoryQueries step@Step{..} idxs = do
  (myIdx, inv) <- readIORef ioInv
  let ordered = prioritiseRemoteInventory members inv idxs
      queries = dedupeInventoryQueries ordered
  forM_ queries \(peer, wanted) ->
    sendAuthenticated step [peer] $ StepMessage myIdx wanted mempty

--------------------------------------------------------------------------------
-- | Message authentication
--------------------------------------------------------------------------------

-- This thing is a bit terrible. It would be easy to make a mistake using it.
getBallotSighash :: BallotData a => a -> Bytes32
getBallotSighash obj = do
  case cast obj of
    Just b -> b
    Nothing -> sha3b $ encode obj

getMessageSigHash :: BallotData i => Step i -> (Maybe StepNum, StepMessage i) -> Bytes32
getMessageSigHash Step{..} obj = sha3b $ encode (processId, obj)

sendAuthenticated :: (BallotData i, Base m)
                  => Step i -> [NodeId] -> StepMessage i -> ConsensusStep i m ()
sendAuthenticated Step{..} peers obj = do
  EthIdent{..} <- bone GetIdentFree
  let payload = (Just stepNum, obj)
  let sighash = getMessageSigHash Step{..} payload
  sig <- signIO ethSecKey sighash
  forM_ peers $ \peer -> do
    bone $ SendRemoteFree peer processId (sig, payload)


-- TODO: Track who sends bad data
authenticate :: (BallotData i, Base m)
             => Step i
             -> (NodeId -> StepMessage i -> ConsensusStep i m ())
             -> AuthenticatedStepMessage i
             -> ConsensusStep i m ()
authenticate step@Step{..} act (RemoteMessage nodeId wsm) = do
  let WrappedStepMessage theirSig sn obj = wsm
  let sighash = getMessageSigHash step (sn, obj)
  addr <- recoverAddr sighash theirSig
  if elem addr members
     then act nodeId obj
     else logWarn $ "Not member or wrong step: " ++ show addr

--------------------------------------------------------------------------------
-- | Pure functions for inventory building
--------------------------------------------------------------------------------

-- | Get a bit array of what inventory we have
inventoryIndex :: [Address] -> Map Address a -> PackedInteger
inventoryIndex members inv =
  let have addr = if Map.member addr inv then 1 else 0
      shiftHave n (i, addr) = n .|. shift (have addr) i
   in foldl shiftHave 0 $ zip [0..] members

-- | Sort remote inventories by how interesting they are and remote inventory we already have
prioritiseRemoteInventory :: [Address] -> Map Address a -> [(b, PackedInteger)] -> [(b, PackedInteger)]
prioritiseRemoteInventory members inv idxs =
  let myIdx = inventoryIndex members inv
      interesting = [(p, i .&. complement myIdx) | (p, i) <- idxs]
   in sortOn (\(_,i) -> popCount i * (-1)) interesting

-- Get queries for remote inventory filtering duplicates
dedupeInventoryQueries :: [(a, PackedInteger)] -> [(a, PackedInteger)]
dedupeInventoryQueries = f 0 where
  f _ [] = []
  f seen ((peer, idx):xs) =
    let wanted = idx .&. complement seen
        rest = f (seen .|. wanted) xs
     in if wanted /= 0
           then (peer, wanted) : rest
           else []

-- Get part of the inventory according to an index
getInventorySubset :: PackedInteger -> [Address] -> Inventory a -> Inventory a
getInventorySubset idx members =
  Map.filterWithKey $
    \k _ -> let Just i = elemIndex k members
             in testBit idx i
