
module Zeno.Consensus.Step where

-- This module deals with exchanging messages and building inventory.
-- The entry point is `runStep`. Each step has a topic, and each member
-- will provide an input. Messages are exhanged until all participants
-- have a full inventory or the step is cancelled.

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.STM (orElse)

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
import           Zeno.Consensus.Types
import           Zeno.Console
import           Zeno.Prelude

import           UnliftIO
import           UnliftIO.Concurrent


inventoryQueryInterval :: Int
inventoryQueryInterval = 200 * 1000

type InventoryMask = Integer

data Step i = Step
  { processId  :: ProcessId
  , ioInv      :: IORef (InventoryMask, Inventory i)
  , members    :: [Address]
  , membersSet :: Set Address
  , yield      :: Inventory i -> Consensus ()
  }


spawnStep :: BallotData i => i -> Collect i o -> Consensus (Process o)
spawnStep obj collect = do

  ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
  let sighash = getBallotSighash obj
  mySig <- sign sk sighash
  let ballot = Ballot myAddr mySig obj

  ConsensusContext{ccParams = ConsensusParams{..},..} <- ask
  roundId <- getRoundId
  processId <- ProcessId . bappend roundId . reFixed <$> getStepEntropy

  let stepName = "step: " ++ show processId

  ioInv <- newIORef (0, mempty)
  
  spawn stepName \process -> do
    yield <- makeYieldUntilDone collect process
    let step = Step processId ioInv members' (Set.fromList members') yield
    registerOnNewPeer $ onNewPeer step
    onInventoryData step True $ Map.singleton myAddr (mySig, obj)
    buildInventoryLoop step


makeYieldUntilDone :: Collect i o -> Process o -> Consensus (Inventory i -> Consensus ())
makeYieldUntilDone collect proc = do
  ref <- newIORef False
  pure \inv -> do
    done <- readIORef ref
    unless done do
      collect inv >>= 
        maybe (pure ()) \r -> do
          writeIORef ref True
          send proc r


buildInventoryLoop :: BallotData i => Step i -> Consensus ()
buildInventoryLoop step@Step{..} = do
  recv <- subscribe processId
  forever do
    delay <- registerDelay inventoryQueryInterval
    fix1 mempty $ \go !idxs -> do
      join do
        atomically do
          orElse
            do readTVar delay >>= checkSTM
               pure $ sendInventoryQueries step $ Map.toList idxs

            do receiveSTM recv <&>
                 authenticate step
                   \peer (StepMessage theirIdx theirReq theirData) -> do
                     onInventoryData step False theirData
                     onInventoryRequest step peer theirReq
                     go $ Map.insert peer theirIdx idxs


onNewPeer :: BallotData i => Step i -> NodeId -> Consensus ()
onNewPeer Step{..} peer = do
  (idx, inv) <- readIORef ioInv
  let msg = StepMessage idx 0 mempty
  sendAuthenticated Step{..} [peer] msg

onInventoryRequest :: BallotData i => Step i -> NodeId -> Integer -> Consensus ()
onInventoryRequest _ _ 0 = mempty
onInventoryRequest step@Step{..} peer theirReq = do
  (myIdx, inv) <- readIORef ioInv
  let subset = getInventorySubset theirReq members inv
  sendAuthenticated step [peer] $ StepMessage myIdx 0 subset


onInventoryData :: BallotData i => Step i -> Bool -> Inventory i -> Consensus ()
onInventoryData _ _ theirInv | Map.null theirInv = mempty
onInventoryData step@Step{..} forwardInv theirInv = do

  let theirIdx = inventoryIndex members theirInv
  (oldIdx, oldInv) <- readIORef ioInv

  unless (0 == theirIdx .&. complement oldIdx) do

    case mergeInventory membersSet oldInv theirInv of
      Left s -> logWarn s

      Right newInv -> do
        let newIdx = theirIdx .|. oldIdx
        writeIORef ioInv (newIdx, newInv)

        yield newInv
        peers <- getPeers
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
    r <- unsafePerformIO $ recoverAddr sighash sig
    when (r /= addr) do
      throwError "Got ballot data with invalid signature"
    pure (sig, val)
    
  -- We shouldn't be receiving data that we already have for a key, ever, ideally.
  -- If anything, maybe we should flag the sender.
  -- Currently, this will no be called since we compare the calculated indexes
  -- before called mergeInventory
  validateExisting _ ours _ = pure ours


sendInventoryQueries :: BallotData i => Step i -> [(NodeId, Integer)] -> Consensus ()
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
getBallotSighash :: (BallotData a, Typeable a) => a -> Bytes32
getBallotSighash obj = do
  case cast obj of
    Just b -> b
    Nothing -> sha3b $ encode obj

getMessageSigHash :: BallotData i => Step i -> (Maybe StepNum, StepMessage i) -> Bytes32
getMessageSigHash Step{..} obj = sha3b $ encode (processId, obj)

sendAuthenticated :: BallotData i
                  => Step i -> [NodeId] -> StepMessage i -> Consensus ()
sendAuthenticated Step{..} peers obj = do
  EthIdent{..} <- asks has
  stepNum <- Just <$> getStepNum
  let sighash = getMessageSigHash Step{..} (stepNum, obj)
  sig <- sign ethSecKey sighash
  forM_ peers $ \peer -> do
    sendRemote peer processId (sig, stepNum, obj)


-- TODO: Track who sends bad data
authenticate :: BallotData i
             => Step i
             -> (NodeId -> StepMessage i -> Consensus ())
             -> AuthenticatedStepMessage i
             -> Consensus ()
authenticate step@Step{..} act (RemoteMessage nodeId wsm) = do
  let WrappedStepMessage theirSig sn obj = wsm
  let sighash = getMessageSigHash step (sn, obj)
  recoverAddr sighash theirSig >>=
    \case
       Right addr ->
         if elem addr members
            then act nodeId obj
            else logWarn $ "Not member or wrong step: " ++ show addr
       Left s -> do
         logWarn $ "Signature recovery failed: " ++ s

--------------------------------------------------------------------------------
-- | Pure functions for inventory building
--------------------------------------------------------------------------------

-- | Get a bit array of what inventory we have
inventoryIndex :: [Address] -> Map Address a -> Integer
inventoryIndex members inv =
  let have addr = if Map.member addr inv then 1 else 0
      shiftHave n (i, addr) = n .|. shift (have addr) i
   in foldl shiftHave 0 $ zip [0..] members

-- | Sort remote inventories by how interesting they are and remote inventory we already have
prioritiseRemoteInventory :: [Address] -> Map Address a -> [(b, Integer)] -> [(b, Integer)]
prioritiseRemoteInventory members inv idxs =
  let myIdx = inventoryIndex members inv
      interesting = [(p, i .&. complement myIdx) | (p, i) <- idxs]
   in sortOn (\(_,i) -> popCount i * (-1)) interesting

-- Get queries for remote inventory filtering duplicates
dedupeInventoryQueries :: [(a, Integer)] -> [(a, Integer)]
dedupeInventoryQueries = f 0 where
  f _ [] = []
  f seen ((peer, idx):xs) =
    let wanted = idx .&. complement seen
        rest = f (seen .|. wanted) xs
     in if wanted /= 0
           then (peer, wanted) : rest
           else []

-- Get part of the inventory according to an index
getInventorySubset :: Integer -> [Address] -> Inventory a -> Inventory a
getInventorySubset idx members =
  Map.filterWithKey $
    \k _ -> let Just i = elemIndex k members
             in testBit idx i
