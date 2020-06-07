{-# LANGUAGE Strict #-}

module Zeno.Consensus.Step
  ( Ballot(..)
  , Inventory
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , spawnStep
  ) where

-- This module deals with exchanging messages and building inventory.
-- The entry point is `runStep`. Each step has a topic, and each member
-- will provide an input. Messages are exhanged until all participants
-- have a full inventory or the step is cancelled.

import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.STM (orElse)

import           Data.Bits
import           Data.Serialize
import qualified Data.Map as Map

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

data Step i = Step
  { processId :: ProcessId
  , tInv      :: TVar (Integer, Inventory i)
  , members   :: [Address]
  , yieldTo   :: Process (Inventory i)
  }


spawnStep :: Serialize i => Ballot i -> Consensus (Process (Inventory i))
spawnStep myBallot = do

  ConsensusContext{ccParams = ConsensusParams{..},..} <- ask
  roundId <- getRoundId
  processId <- ProcessId . bappend roundId . reFixed <$> getStepEntropy

  let (Ballot myAddr mySig myData) = myBallot
      stepName = "step: " ++ show processId

  tInv <- newTVarIO (0, mempty)
  
  spawn stepName \process -> do
    let step = Step processId tInv members' process
    registerOnNewPeer $ onNewPeer step
    onInventoryData step True $ Map.singleton myAddr (mySig, myData)
    buildInventoryLoop step


buildInventoryLoop :: Serialize i => Step i -> Consensus ()
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


onNewPeer :: Serialize i => Step i -> NodeId -> Consensus ()
onNewPeer Step{..} peer = do
  (idx, inv) <- readTVarIO tInv
  let msg = StepMessage idx 0 mempty
  sendAuthenticated Step{..} [peer] msg

onInventoryRequest :: Serialize i => Step i -> NodeId -> Integer -> Consensus ()
onInventoryRequest _ _ 0 = mempty
onInventoryRequest step@Step{..} peer theirReq = do
  (myIdx, inv) <- readTVarIO tInv
  let subset = getInventorySubset theirReq members inv
  sendAuthenticated step [peer] $ StepMessage myIdx 0 subset

onInventoryData :: Serialize i => Step i -> Bool -> Inventory i -> Consensus ()
onInventoryData _ _ theirInv | Map.null theirInv = mempty
onInventoryData step@Step{..} forwardInv theirInv = do

  -- TODO: Authenticate all the inventory we don't have.
  
  mr <-
    atomically do
      let theirIdx = inventoryIndex members theirInv
      (oldIdx, oldInv) <- readTVar tInv
      if 0 == theirIdx .&. complement oldIdx
        then pure Nothing
        else do
          let newInv = Map.union oldInv theirInv
              newIdx = inventoryIndex members newInv
          writeTVar tInv (newIdx, newInv)
          pure $ Just (newIdx, newInv)

  forM_ mr \(idx, inv) -> do
    send yieldTo inv
    peers <- getPeers
    let fwdInv = if forwardInv then inv else mempty
    sendAuthenticated step peers $ StepMessage idx 0 fwdInv


sendInventoryQueries :: Serialize i => Step i -> [(NodeId, Integer)] -> Consensus ()
sendInventoryQueries step@Step{..} idxs = do
  (myIdx, inv) <- readTVarIO tInv
  let ordered = prioritiseRemoteInventory members inv idxs
      queries = dedupeInventoryQueries ordered
  forM_ queries \(peer, wanted) ->
    sendAuthenticated step [peer] $ StepMessage myIdx wanted mempty

--------------------------------------------------------------------------------
-- | Message authentication
--------------------------------------------------------------------------------

getMessageToSign :: Serialize i => Step i -> (Maybe StepNum, StepMessage i) -> Bytes32
getMessageToSign Step{..} obj = sha3b $ encode (processId, obj)

sendAuthenticated :: Serialize o
                  => Step o -> [NodeId] -> StepMessage o -> Consensus ()
sendAuthenticated Step{..} peers obj = do
  EthIdent{..} <- asks has
  stepNum <- Just <$> getStepNum
  let sighash = getMessageToSign Step{..} (stepNum, obj)
  let sig = sign ethSecKey sighash
  forM_ peers $ \peer -> do
    let act = sendRemote peer processId (sig, stepNum, obj)
    act
    -- If there are errors from secp256k1 try:
    -- catchAny
    --   do deepseq sig act
    --   \e -> do
    --     traceShowM sighash
    --     throwIO e


-- TODO: Track who sends bad data
authenticate :: Serialize i
             => Step i
             -> (NodeId -> StepMessage i -> Consensus ())
             -> AuthenticatedStepMessage i
             -> Consensus ()
authenticate step@Step{..} act (RemoteMessage nodeId wsm) = do
  let WrappedStepMessage theirSig sn obj = wsm
  let sighash = getMessageToSign step (sn, obj)
  case recoverAddr sighash theirSig of
       Just addr ->
         if elem addr members
            then act nodeId obj
            else logWarn $ "Not member or wrong step: " ++ show addr
       Nothing -> do
         logWarn "Signature recovery failed"

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
