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

data Step i = Step
  { topic   :: ProcessId
  , tInv    :: TVar (Inventory i)
  , members :: [Address]
  , yieldTo :: ProcessId
  , mySig   :: CompactRecSig
  , message :: Msg
  }


spawnStep :: forall i. (Sendable i)
          => Msg -> Ballot i -> [Address] -> (Consensus i (ProcessHandle (RemoteMessage LazyByteString)))
spawnStep message myBallot members = do

  yieldTo <- getMyPid

  -- Build the step context
  tInv <- newTVarIO Map.empty
  -- Hash the message as the topic so it's obfuscated
  let topic = serviceId $ getMsg message
      (Ballot myAddr mySig myData) = myBallot
      step = Step topic tInv members yieldTo mySig message


  spawnChildNamed topic do
    -- Spawn our inventory builder
    builder <- procId <$> spawnChild (inventoryBuilder step)

    -- Register so that we get new peer events
    P2P.onNewPeer $ onNewPeer step

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
                  sendAuthenticated step peer $ InventoryData subset

                InventoryData inv -> onInventoryData step inv
                -- TODO: new peer


onNewPeer :: forall i i2 p. (Sendable i, Process i2 p) => Step i -> NodeId -> p ()
onNewPeer Step{..} peer = do
  inv <- readTVarIO tInv
  let idx = inventoryIndex members inv
  sendAuthenticated Step{..} peer (InventoryIndex idx :: StepMessage a)

onInventoryData :: forall i. Sendable i => Step i -> Inventory i -> Consensus i ()
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
    traceM "sendPeers"
    P2P.sendPeers topic (mySig, InventoryIndex idx :: StepMessage i)


-- | Inventory builder, continually builds and dispatches queries for remote inventory

inventoryBuilder :: Sendable i => Step i -> Consensus i ()
inventoryBuilder step@Step{..} = do
  forever do
    getInventoryQueries step >>=
      mapM_ \(peer, wanted) ->
        sendAuthenticated step peer $ GetInventory wanted

    threadDelay $ 100 * 1000


getInventoryQueries :: (Sendable i) => Step i -> Consensus i [(NodeId, Integer)]
getInventoryQueries Step{..} = do
  idxs <- recvAll
  inv <- readTVarIO tInv
  let ordered = prioritiseRemoteInventory members inv idxs
  pure $ dedupeInventoryQueries ordered
  where
  recvAll = receiveMaybeRemote >>= maybe (pure []) (\a -> (rMsg a:) <$> recvAll)

--------------------------------------------------------------------------------
-- | Message authentication
--------------------------------------------------------------------------------

sendAuthenticated :: (Sendable i, Process i2 p) => Step i -> NodeId -> StepMessage i -> p ()
sendAuthenticated Step{..} peer msg = do
  traceShowM ("sendAuthenticated", peer)
  sendRemote peer topic (mySig, msg)

-- TODO: Track who sends bad data
authenticate :: Step i -> (b -> Consensus i ()) -> (CompactRecSig, b) -> Consensus i ()
authenticate step@Step{..} act (theirSig, obj) =
  case recoverAddr message theirSig of
       Just addr ->
         if elem addr members
            then act obj
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
