{-# LANGUAGE MonoLocalBinds #-}

module Zeno.Consensus.Step
  ( Ballot(..)
  , Inventory
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , runStep
  ) where

import           Control.Monad

import           Control.Distributed.Process
import           Control.Distributed.Process.Serializable (Serializable)

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


runStep :: forall a. Serializable a
        => Msg
        -> Ballot a
        -> [Address]
        -> (Inventory a -> Process ())
        -> Process ()
runStep message myBallot members yield = do
  -- Hash the message as the topic so it's obfuscated
  let topic = asString $ getMsg $ hashMsg $ getMsg message
      (Ballot myAddr mySig myData) = myBallot

  mInv <- newMVar Map.empty
  myPid <- getSelfPid
  let step = Step topic mInv members myPid yield mySig message
  builder <- spawnLocalLink $ buildInventory step

  nsend P2P.peerListenerService myPid
  register topic myPid
  send myPid (mySig, InventoryData $ Map.singleton myAddr (mySig, myData))
  forever $ receiveWait [ match $ onInventoryIndex builder step
                        , match $ onGetInventory step
                        , match $ onInventoryData step
                        , match $ onNewPeer step
                        ]

onNewPeer :: Step a -> P2P.NewPeer -> Process ()
onNewPeer Step{..} (P2P.NewPeer nodeId) = do
  inv <- readMVar mInv
  let idx = inventoryIndex members inv
  nsendRemote nodeId topic (mySig, InventoryIndex parent idx)

authenticate :: (Step a -> b -> Process ()) -> Step a -> (CompactRecSig, b) -> Process ()
authenticate act step@Step{..} (theirSig, obj) =
  case recoverAddr message theirSig of
       Just addr ->
         if elem addr members
            then act step obj
            else say $ "Not member or wrong step: " ++ show addr
       Nothing -> do
         say "Signature recovery failed"

onInventoryIndex :: ProcessId -> Step a -> Authenticated InventoryIndex -> Process ()
onInventoryIndex builder = authenticate $ \Step{..} (InventoryIndex peer theirIdx) ->
  send builder (peer, theirIdx)

onGetInventory :: Serializable a => Step a -> Authenticated GetInventory -> Process ()
onGetInventory = authenticate $ \Step{..} (GetInventory peer idx) -> do
  inv <- readMVar mInv
  let idx = inventoryIndex members inv
  let subset = getInventorySubset idx members inv
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

buildInventory :: forall a. Serializable a => Step a -> Process ()
buildInventory Step{..} = do
  forever $ do
    -- Stage 1: Take a few remote indexes and send requests
    idxs <- recvAll :: Process [(ProcessId, Integer)]
    ordered <- prioritiseRemoteInventory members <$> readMVar mInv <*> pure idxs
    let queries = dedupeInventoryQueries ordered
    forM_ queries $ \(peer, wanted) -> do
      send peer (mySig, GetInventory parent wanted)
    -- Stage 2: Take a nap
    threadDelay $ 100 * 1000

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
   in sortOn (\(pid,i) -> popCount i * (-1)) interesting

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

recvAll :: Serializable a => Process [a]
recvAll = expectTimeout 0 >>= maybe (pure []) (\a -> (a:) <$> recvAll)

-- Utility

--signMsg :: Serializable o => Step a -> o -> Authenticated o
--signMsg Step{..} o =
--  let message = hashMsg $ encode o <> getMsg topic
--   in (sign message, o)
    
