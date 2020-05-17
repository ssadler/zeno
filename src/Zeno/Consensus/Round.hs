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
import           Zeno.Prelude.Lifted (getCurrentTime, timeDelta)
import qualified Zeno.Consensus.P2P as P2P
import           Zeno.Consensus.Step
import           Zeno.Consensus.Types
import           Zeno.Consensus.Utils

import           UnliftIO.STM


-- Run round ------------------------------------------------------------------

runConsensus :: (Serializable a, Has ConsensusNode r) => ConsensusParams
             -> a -> Consensus b -> Zeno r b
runConsensus params@ConsensusParams{..} topicData act = do
  atomically $ writeTVar mtopic $ hashMsg $ toStrict $ Bin.encode topicData
  let act' = runReaderT act params
  ConsensusNode node <- asks $ has
  liftIO $ do
    handoff <- newEmptyTMVarIO
    runProcess node $ do
      _ <- spawnLocalLink P2P.peerNotifier
      act' >>= atomically . putTMVar handoff
    atomically $ takeTMVar handoff

-- Coordinate Round -----------------------------------------------------------

-- | step initiates the procedure of exchanging signed messages.
--   The messages contain a signature which is based on 
--   
step' :: Serializable a => Collect a -> a -> Consensus (Inventory a)
step' collect obj = do
  ConsensusParams members (EthIdent sk myAddr) timeout mtopic <- ask
  topic <- readTVarIO mtopic
  let sig = sign sk topic
  let ballot = Ballot myAddr sig obj
  lift $ do
    (send, recv) <- newChan
    _ <- spawnLocalLink $ runStep topic ballot members $ sendChan send
    collect recv timeout members

stepWithTopic :: Serializable a => Topic -> Collect a -> a -> Consensus (Inventory a)
stepWithTopic topic collect o = do
  asks mtopic >>= atomically . flip writeTVar topic
  step' collect o

step :: Serializable a => Collect a -> a -> Consensus (Inventory a)
step collect o = permuteTopic () >> step' collect o

-- 1. Determine a starting proposer
-- 2. Try to get a proposal from them
-- 3. If there's a timeout, move to the next proposer
propose :: forall a. Serializable a => Consensus a -> Consensus (Ballot a)
propose mObj = do
  determineProposers >>= go
    where
      go :: [(Address, Bool)] -> Consensus (Ballot a)
      go [] = throw $ ConsensusTimeout "Ran out of proposers"
      go ((pAddr, isMe):xs) = do

        permuteTopic pAddr

        let nextProposer (ConsensusTimeout _) = do
              lift $ say $ "Timeout collecting for proposer: " ++ show pAddr
              go xs
            nextProposer e = throw e

        obj <-
          if isMe
             then (lift $ say "I am the chosen one.") >> (Just <$> mObj)
             else pure Nothing

        handle nextProposer $ do
          withTimeout (5 * 1000000) $ do
            results <- step' (collectMembers [pAddr]) obj
            case Map.lookup pAddr results of
                 Just (s, Just obj2) -> pure $ Ballot pAddr s obj2
                 _                   -> do
                   lift $ say $ "Mischief: missing proposal from: " ++ show pAddr
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
  ConsensusParams members (EthIdent _ myAddr)  _ mtopic <- ask
  let msg2sum = sum . map fromIntegral . BS.unpack . getMsg
  topic <- readTVarIO mtopic
  let i = mod (msg2sum topic) (length members)
      proposers = take 3 $ drop i $ cycle members
  pure $ [(p, p == myAddr) | p <- proposers]

permuteTopic :: Bin.Binary a => a -> Consensus Topic
permuteTopic key = do
  tv <- asks mtopic
  atomically do
    cur <- readTVar tv <&> getMsg
    let r = hashMsg $ toStrict $ Bin.encode (key, cur)
    writeTVar tv r
    pure r

-- Check Majority -------------------------------------------------------------

-- TODO: Refactor so that test is provided rather than collector

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
    let us = timeout - min timeout d
    minv <- receiveChanTimeout us recv
    case minv of
         Nothing -> throw $ ConsensusTimeout $ "collect timeout after %i seconds" % quot timeout 1000000
         Just inv | test members inv -> pure inv
         _ -> f

haveMajority :: [Address] -> Inventory a -> Bool
haveMajority members inv =
   length inv >= majorityThreshold (length members)

majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
