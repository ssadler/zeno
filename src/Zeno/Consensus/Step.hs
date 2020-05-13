{-# LANGUAGE MonoLocalBinds #-}

module Zeno.Consensus.Step
  ( Ballot(..)
  , Inventory
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , runStep
  ) where

-- This module deals with exchanging messages and building inventory.
-- The entry point is `runStep`. Each step has a topic, and each member
-- will provide an input. Messages are exhanged until all participants
-- have a full inventory.

import           Control.Monad

import           Network.NQE

import           Data.Binary
import           Data.Bits
import qualified Data.Map as Map

import           GHC.Generics (Generic)

import           Network.Ethereum.Crypto
import qualified Zeno.Consensus.P2P as P2P
import           Zeno.Consensus.Types
import           Zeno.Consensus.Utils
import           Zeno.Prelude.Lifted
import           Zeno.Prelude


-- TODO: All messages should be authenticated


data InventoryIndex = InventoryIndex ProcessId Integer deriving (Generic)
data GetInventory = GetInventory ProcessId Integer deriving (Generic)
data InventoryData a = InventoryData (Inventory a) deriving (Generic)

instance Binary InventoryIndex
instance Binary GetInventory
instance Binary a => Binary (InventoryData a)


data Step a = Step
  { topic :: String
  , mInv :: MVar (Inventory a)
  , members :: [Address]
  , parent :: ProcessId
  , yield :: (Inventory a -> Process ())
  , mySig :: CompactRecSig
  , message :: Msg
  }


runStep :: forall a. Sendable a
        => Msg
        -> Ballot a
        -> [Address]
        -> (Inventory a -> Process ())
        -> Process ()
runStep message myBallot members yield = do

  -- Build the step context
  mInv <- newMVar Map.empty
  myPid <- getSelfPid
  -- Hash the message as the topic so it's obfuscated
  let topic = asString $ getMsg $ hashMsg $ getMsg message
      (Ballot myAddr mySig myData) = myBallot
      step = Step topic mInv members myPid yield mySig message

  -- Spawn our inventory builder
  builder <- spawnLocalLink $ inventoryBuilder step

  -- Register so that we get new peer events
  nsend P2P.peerListenerService myPid

  -- Register to current topic so we will get messages
  register topic myPid

  -- Dispatching own inventory to self kicks off the process of propagating it
  send myPid (mySig, InventoryData $ Map.singleton myAddr (mySig, myData))

  forever do
    receiveWait
      [ match $ onInventoryIndex builder step
      , match $ onGetInventory step
      , match $ onInventoryData step
      , match $ onNewPeer step
      ]

onNewPeer :: Step a -> P2P.NewPeer -> Process ()
onNewPeer Step{..} (P2P.NewPeer nodeId) = do
  inv <- readMVar mInv
  let idx = inventoryIndex members inv
  nsendRemote nodeId topic (mySig, InventoryIndex parent idx)

onInventoryIndex :: ProcessId -> Step a -> Authenticated InventoryIndex -> Process ()
onInventoryIndex builder = authenticate $
  \Step{..} (InventoryIndex peer theirIdx) -> send builder (peer, theirIdx)

onGetInventory :: Sendable a => Step a -> Authenticated GetInventory -> Process ()
onGetInventory = authenticate $ \Step{..} (GetInventory peer wanted) -> do
  inv <- readMVar mInv
  let subset = getInventorySubset wanted members inv
  send peer (mySig, InventoryData subset)

onInventoryData :: Step a -> Authenticated (InventoryData a) -> Process ()
onInventoryData = authenticate $ \Step{..} (InventoryData theirInv) -> do
  oldIdx <- inventoryIndex members <$> readMVar mInv
  -- TODO: authenticate
  modifyMVar_ mInv $ pure . Map.union theirInv

  inv <- readMVar mInv
  let idx = inventoryIndex members inv
  when (0 /= idx .&. complement oldIdx) $ do
    yield inv
    P2P.nsendPeers topic (mySig, InventoryIndex parent idx)


authenticate :: (Step a -> b -> Process ()) -> Step a -> (CompactRecSig, b) -> Process ()
authenticate act step@Step{..} (theirSig, obj) =
  case recoverAddr message theirSig of
       Just addr ->
         if elem addr members
            then act step obj
            else say $ "Not member or wrong step: " ++ show addr
       Nothing -> do
         say "Signature recovery failed"


-- | Inventory builder, continually builds and dispatches queries for remote inventory

inventoryBuilder :: forall a. Sendable a => Step a -> Process ()
inventoryBuilder step@Step{..} = do
  forever $ do
    getInventoryQueries step >>=
      mapM_ (\(peer, wanted) -> do
        send peer (mySig, GetInventory parent wanted))
    
    threadDelay $ 100 * 1000


-- | 
getInventoryQueries :: Step a -> Process [(ProcessId, Integer)]
getInventoryQueries Step{..} = do
  idxs <- recvAll
  inv <- readMVar mInv
  let ordered = prioritiseRemoteInventory members inv idxs
  pure $ dedupeInventoryQueries ordered
  where
  recvAll = expectTimeout 0 >>= maybe (pure []) (\a -> (a:) <$> recvAll)


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
