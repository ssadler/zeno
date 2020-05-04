{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zeno.Consensus.Round
  ( ConsensusException(..)
  , step
  , stepWithTopic
  , propose
  , say
  , collectMembers
  , collectMajority
  , collectThreshold
  , haveMajority
  , collectGeneric
  , runConsensus
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , majorityThreshold
  ) where

import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import           Control.Distributed.Process.Serializable (Serializable)

import qualified Data.ByteString as BS
import qualified Data.Binary as Bin
import qualified Data.Map as Map

import           Network.Ethereum.Crypto

import           Zeno.Prelude
import           Zeno.Prelude.Lifted
import qualified Zeno.Consensus.P2P as P2P
import           Zeno.Consensus.Step
import           Zeno.Consensus.Types
import           Zeno.Consensus.Utils


-- Run round ------------------------------------------------------------------

runConsensus :: (Serializable a, Has ConsensusNode r) => ConsensusParams
             -> a -> Consensus b -> Zeno r b
runConsensus params topicData act = do
  let topic = hashMsg $ toStrict $ Bin.encode topicData
      act' = evalStateT (runReaderT act params) topic
  ConsensusNode node <- asks $ has
  liftIO $ do
    handoff <- newEmptyMVar
    runProcess node $ do
      _ <- spawnLocalLink P2P.peerNotifier
      act' >>= putMVar handoff
    takeMVar handoff

-- Coordinate Round -----------------------------------------------------------

-- | step initiates the procedure of exchanging signed messages.
--   The messages contain a signature which is based on 
--   
step' :: Serializable a => Collect a -> a -> Consensus (Inventory a)
step' collect obj = do
  topic <- get
  ConsensusParams members (EthIdent sk myAddr) timeout <- ask
  let sig = sign sk topic
  let ballot = Ballot myAddr sig obj
  lift $ lift $ do
    (send, recv) <- newChan
    _ <- spawnLocalLink $ runStep topic ballot members $ sendChan send
    collect recv timeout members

stepWithTopic :: Serializable a => Topic -> Collect a -> a -> Consensus (Inventory a)
stepWithTopic topic collect o = put topic >> step' collect o

step :: Serializable a => Collect a -> a -> Consensus (Inventory a)
step collect o = permuteTopic >> step' collect o

-- 1. Determine a starting proposer
-- 2. Try to get a proposal from them
-- 3. If there's a timeout, move to the next proposer
propose :: forall a. Serializable a => Consensus a -> Consensus a
propose mObj = do
  determineProposers >>= go
    where
      go :: [(Address, Bool)] -> Consensus a
      go [] = throw $ ConsensusTimeout "Ran out of proposers"
      go ((pAddr, isMe):xs) = do

        let nextProposer (ConsensusTimeout _) = do
              _ <- permuteTopic  -- Will have been lost due to exception
              lift $ lift $ say $ "Timeout collecting for proposer: " ++ show pAddr
              go xs
            nextProposer e = throw e

        obj <- if isMe then Just <$> mObj else pure Nothing

        handle nextProposer $ do
          withTimeout (5 * 1000000) $ do
            results <- step' (collectMembers [pAddr]) obj
            case Map.lookup pAddr results of
                 Just (_, Just obj2) -> pure obj2
                 _                   -> do
                   lift $ lift $ say $ "Mischief: missing proposal from: " ++ show pAddr
                   throw (ConsensusMischief $ printf "Missing proposal from %s" $ show pAddr)

determineProposers :: Consensus [(Address, Bool)]
determineProposers = do
  {- This gives fairly good distribution:
  import hashlib
  dist = [0] * 64
  for i in xrange(100000):
      m = hashlib.sha256(str(i))
      d = sum(map(ord, m.digest()))
      dist[d%64] += 1
  print dist
  -}
  ConsensusParams members (EthIdent _ myAddr)  _ <- ask
  let msg2sum = sum . map fromIntegral . BS.unpack . getMsg
  topic <- get
  let i = mod (msg2sum topic) (length members)
      proposers = take 3 $ drop i $ cycle members
  pure $ [(p, p == myAddr) | p <- proposers]

permuteTopic :: Consensus Topic
permuteTopic = do
  out <- get
  put $ hashMsg $ getMsg out
  pure out

-- Check Majority -------------------------------------------------------------

-- | Collects a majority
collectMajority :: Serializable a => Collect a
collectMajority = collectGeneric haveMajority

-- | Wait for results from specific members
collectMembers :: Serializable a => [Address] -> Collect a
collectMembers addrs = collectGeneric $ \_ -> allSigned
  where allSigned inv = all id [Map.member a inv | a <- addrs]

collectThreshold :: Serializable a => Int -> Collect a
collectThreshold t = collectGeneric $ \_ inv -> length inv == t

collectGeneric :: Serializable a => ([Address] -> Inventory a -> Bool) -> Collect a
collectGeneric test recv timeout members = do
  startTime <- getCurrentTime
  fix $ \f -> do
    d <- timeDelta startTime
    let us = max 0 $ timeout - d
    minv <- receiveChanTimeout us recv
    case minv of
         Nothing -> throw (ConsensusTimeout "collect timeout")
         Just inv | test members inv -> pure inv
         _ -> f

haveMajority :: [Address] -> Inventory a -> Bool
haveMajority members inv =
   length inv >= majorityThreshold (length members)

majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
