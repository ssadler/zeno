{-# LANGUAGE MonoLocalBinds #-}

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
-- have a full inventory.

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
inventoryQueryInterval = 100 * 1000

data Step i = Step
  { processId :: ProcessId
  , tInv      :: TVar (Inventory i)
  , members   :: [Address]
  , yieldTo   :: Process (Inventory i)
  }

spawnStep :: forall a i. Serialize i => Ballot i -> Consensus (Process (Inventory i))
spawnStep myBallot = do

  ConsensusContext{ccParams = ConsensusParams{..},..} <- ask
  stepNum <- getStepNum
  roundId <- getRoundId

  let (Ballot myAddr mySig myData) = myBallot
      suffix = toFixed $ sha3' $ encode (ccEntropy, stepNum)
      processId = ProcessId $ roundId `bappend` suffix
      stepName = "step: " ++ show processId

  spawn stepName \process -> do
    tInv <- newTVarIO Map.empty
    let step = Step processId tInv members' process
    registerOnNewPeer $ onNewPeer step
    -- | This kick-starts the process
    onInventoryData step $ Map.singleton myAddr (mySig, myData)
    buildInventoryLoop step processId


buildInventoryLoop :: Serialize i => Step i -> ProcessId -> Consensus ()
buildInventoryLoop step@Step{..} pid = do
  recv <- subscribe pid
  forever do
    delay <- registerDelay inventoryQueryInterval
    fix1 [] $ \go !idxs -> do
      join do
        atomically do
          orElse
            do readTVar delay >>= checkSTM
               pure $ sendInventoryQueries step idxs

            do receiveSTM recv <&>
                 authenticate step
                   \peer ->
                     \case
                       InventoryIndex theirIdx -> do
                         go $ (peer, theirIdx) : idxs

                       GetInventory wanted -> do
                         inv <- readTVarIO tInv
                         let subset = getInventorySubset wanted members inv
                         sendAuthenticated step [peer] $ InventoryData subset
                         go idxs

                       InventoryData inv -> do
                         onInventoryData step inv
                         go idxs


onNewPeer :: forall i. Serialize i => Step i -> NodeId -> Consensus ()
onNewPeer Step{..} peer = do
  inv <- readTVarIO tInv
  let idx = inventoryIndex members inv
  sendAuthenticated Step{..} [peer] (InventoryIndex idx :: StepMessage i)

onInventoryData :: forall i. Serialize i => Step i -> Inventory i -> Consensus ()
onInventoryData step@Step{..} theirInv = do

  -- TODO: Authenticate all the inventory we don't have.

  oldIdx <- inventoryIndex members <$> readTVarIO tInv
  atomically do
    modifyTVar tInv $ Map.union theirInv

  inv <- readTVarIO tInv
  let idx = inventoryIndex members inv
  when (0 /= idx .&. complement oldIdx) do
    send yieldTo inv
    peers <- getPeers
    sendAuthenticated step peers (InventoryIndex idx :: StepMessage i)

sendInventoryQueries :: Serialize i => Step i -> [(NodeId, Integer)] -> Consensus ()
sendInventoryQueries step@Step{..} idxs = do
  inv <- readTVarIO tInv
  let ordered = prioritiseRemoteInventory members inv idxs
      queries = dedupeInventoryQueries ordered

  forM_ queries \(peer, wanted) ->
    sendAuthenticated step [peer] $ GetInventory wanted

--------------------------------------------------------------------------------
-- | Message authentication
--------------------------------------------------------------------------------

getMessageToSign :: Serialize o => Step o -> StepMessage o -> Bytes32
getMessageToSign Step{..} obj = sha3b $ encode (processId, obj)

sendAuthenticated :: Serialize o
                  => Step o -> [NodeId] -> StepMessage o -> Consensus ()
sendAuthenticated Step{..} peers obj = do
  EthIdent{..} <- asks has
  let sig = sign ethSecKey $ getMessageToSign Step{..} obj
  forM_ peers $ \peer -> sendRemote peer processId (sig, obj)

-- TODO: Track who sends bad data
authenticate :: Serialize i
             => Step i
             -> (NodeId -> StepMessage i -> Consensus ())
             -> AuthenticatedStepMessage i
             -> Consensus ()
authenticate step@Step{..} act (RemoteMessage nodeId (theirSig, obj)) = do
  let message = getMessageToSign step obj
  case recoverAddr message theirSig of
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
    \k _ -> let Just i = elemIndex k members   -- Bit of a murphy here
             in testBit idx i
