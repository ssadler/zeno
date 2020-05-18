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

import           Data.Binary
import           Data.Bits
import qualified Data.Map as Map

import           GHC.Generics (Generic)

import           Network.Distributed
import           Network.Ethereum.Crypto
import qualified Zeno.Consensus.P2P as P2P
import           Zeno.Consensus.Types
--import           Zeno.Prelude.Lifted
import           Zeno.Prelude

import           UnliftIO
import           UnliftIO.Concurrent


-- TODO: All messages should be authenticated


data StepMessage a =
    InventoryIndex Integer
  | GetInventory Integer
  | InventoryData (Inventory a)
  deriving (Generic)

instance Sendable a => Binary (StepMessage a)

data Step a = Step
  { topic   :: ProcessId
  , tInv    :: TVar (Inventory a)
  , members :: [Address]
  , yieldTo :: ProcessId
  , mySig   :: CompactRecSig
  , message :: Msg
  }


spawnStep :: forall a. (Sendable a, Typeable a)
        => Msg
        -> Ballot a
        -> [Address]
        -> Consensus ()
spawnStep message myBallot members = do

  yieldTo <- getMyPid

  -- Build the step context
  tInv <- newTVarIO Map.empty
  -- Hash the message as the topic so it's obfuscated
  let topic = ProcessId $ getMsg $ hashMsg $ getMsg message
      (Ballot myAddr mySig myData) = myBallot
      step = Step topic tInv members yieldTo mySig message


  spawnChildNamed topic do
    -- Spawn our inventory builder
    builder <- spawnChild $ inventoryBuilder step

    -- Register so that we get new peer events
    -- TODO nsend P2P.peerListenerService myPid


    onInventoryData step $ Map.singleton myAddr (mySig, myData)

    forever do
      receiveWait >>=
        withRemoteMessage do
          \peer ->
            authenticate step $
              \case
                InventoryIndex theirIdx -> do
                  send builder (peer, theirIdx)
                
                GetInventory wanted -> do
                  inv <- readTVarIO tInv
                  let subset = getInventorySubset wanted members inv
                  sendRemote peer topic (mySig, InventoryData subset)

                InventoryData inv -> onInventoryData step inv
                -- TODO: new peer


onNewPeer :: forall a. Sendable a => Step a -> P2P.NewPeer -> Consensus ()
onNewPeer Step{..} (P2P.NewPeer nodeId) = do
  inv <- readTVarIO tInv
  let idx = inventoryIndex members inv
  sendRemote nodeId topic (mySig, InventoryIndex idx :: StepMessage a)

onInventoryData :: forall a. Sendable a => Step a -> Inventory a -> Consensus ()
onInventoryData Step{..} theirInv = do

  -- TODO, very important:
  -- Authenticate all the inventory we don't have.

  oldIdx <- inventoryIndex members <$> readTVarIO tInv
  atomically do
    modifyTVar tInv $ Map.union theirInv

  inv <- readTVarIO tInv
  let idx = inventoryIndex members inv
  when (0 /= idx .&. complement oldIdx) $ do
    send yieldTo inv
    P2P.sendPeers topic (mySig, InventoryIndex idx :: StepMessage a)


authenticate :: Step a -> (b -> Consensus ()) -> (CompactRecSig, b) -> Consensus ()
authenticate step@Step{..} act (theirSig, obj) =
  case recoverAddr message theirSig of
       Just addr ->
         if elem addr members
            then act obj
            else say $ "Not member or wrong step: " ++ show addr
       Nothing -> do
         say "Signature recovery failed"

say = error "say"


-- | Inventory builder, continually builds and dispatches queries for remote inventory

inventoryBuilder :: forall a. Sendable a => Step a -> Consensus ()
inventoryBuilder step@Step{..} = do
  forever $ do
    getInventoryQueries step >>=
      mapM_ (\(peer, wanted) -> do
        sendRemote peer topic (mySig, GetInventory wanted :: StepMessage a))
    
    threadDelay $ 100 * 1000


getInventoryQueries :: Step a -> Consensus [(NodeId, Integer)]
getInventoryQueries Step{..} = do
  idxs <- recvAll
  inv <- readTVarIO tInv
  let ordered = prioritiseRemoteInventory members inv idxs
  pure $ dedupeInventoryQueries ordered
  where
  recvAll = receiveMaybe >>= maybe (pure []) (\a -> (a:) <$> recvAll)


-- | Pure functions for inventory building ------------------------------------

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
dedupeInventoryQueries = f 0
  where f seen ((peer, idx):xs) =
          let wanted = idx .&. complement seen
              rest = f (seen .|. wanted) xs
           in if wanted /= 0
                 then (peer, wanted) : rest
                 else []
        f _ [] = []

-- Get part of the inventory according to an index
getInventorySubset :: Integer -> [Address] -> Inventory a -> Inventory a
getInventorySubset idx members =
  Map.filterWithKey $
    \k _ -> let Just i = elemIndex k members
             in testBit idx i
