{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zeno.Consensus.Round
  ( ConsensusException(..)
  , step
  , stepWithTopic
  , propose
  , collectMembers
  , collectMajority
  , collectThreshold
  , runConsensus
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , majorityThreshold
  ) where

import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.ByteString as BS
import qualified Data.Binary as Bin
import           Data.Binary (Binary)
import qualified Data.Map as Map
import           Data.Time.Clock

import           Network.Ethereum.Crypto
import           Zeno.Process

import           Zeno.Prelude
import           Zeno.Consensus.P2P
import           Zeno.Consensus.Step
import           Zeno.Consensus.Types

import           UnliftIO.STM


-- Run round ------------------------------------------------------------------

runConsensus :: (Binary a, Has ConsensusNode r) => ConsensusParams
             -> a -> Consensus b -> Zeno r b
runConsensus params@ConsensusParams{..} topicData act = do
  atomically $ writeTVar mtopic $ hashMsg $ toStrict $ Bin.encode topicData
  cn <- asks has
  let ctx = ConsensusContext cn params
  withZeno (\_ -> ctx) do
    withCloseResources act


-- Coordinate Round -----------------------------------------------------------


-- | Step initiates the procedure of exchanging signed messages.
-- | The step is run in a separate thread, and this thread will wait
-- | until the collect condition has been fulfilled.
step' :: Sendable a => Collect a -> a -> Consensus (Inventory a)
step' collect obj = do
  ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
  topic <- readTVarIO mtopic
  let sig = sign sk topic
  let ballot = Ballot myAddr sig obj

  recv <- spawnStep topic ballot members'

  startTime <- liftIO getCurrentTime
  fix $ \f -> do
    d <- timeDelta startTime
    let us = timeout' - min timeout' d
    minv <- receiveTimeout recv us
    case minv of
      Nothing -> throwIO $ errTimeout timeout'
      Just inv -> do
        pass <- collect inv
        if pass then pure inv else f
  where
  errTimeout t = ConsensusTimeout ("Timeout after %i seconds" % quot t 1000000)

stepWithTopic :: Sendable a => Topic -> Collect a -> a -> Consensus (Inventory a)
stepWithTopic topic collect o = do
  asks (mtopic . ccParams) >>= atomically . flip writeTVar topic
  step' collect o

step :: Sendable a => Collect a -> a -> Consensus (Inventory a)
step collect o = permuteTopic () >> step' collect o

-- 1. Determine a starting proposer
-- 2. Try to get a proposal from them
-- 3. If there's a timeout, move to the next proposer
propose :: forall a. Sendable a => Consensus a -> Consensus (Ballot a)
propose mObj = do
  determineProposers >>= go
    where
      go :: [(Address, Bool)] -> Consensus (Ballot a)
      go [] = throwIO $ ConsensusTimeout "Ran out of proposers"
      go ((pAddr, isMe):xs) = do

        permuteTopic pAddr

        let nextProposer (ConsensusTimeout _) = do
              logInfo $ "Timeout collecting for proposer: " ++ show pAddr
              go xs
            nextProposer e = throwIO e

        obj <-
          if isMe
             then logInfo "I am the chosen one." >> (Just <$> mObj)
             else pure Nothing

        handle nextProposer $ do
          withTimeout (5 * 1000000) $ do
            results <- step' (collectMembers [pAddr]) obj
            case Map.lookup pAddr results of
                 Just (s, Just obj2) -> pure $ Ballot pAddr s obj2
                 _                   -> do
                   logWarn $ "Mischief: missing proposal from: " ++ show pAddr
                   throwIO (ConsensusMischief $ printf "Missing proposal from %s" $ show pAddr)

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
  ConsensusParams members (EthIdent _ myAddr)  _ mtopic <- asks ccParams
  let msg2sum = sum . map fromIntegral . BS.unpack . getMsg
  topic <- readTVarIO mtopic
  let i = mod (msg2sum topic) (length members)
      proposers = take 3 $ drop i $ cycle members
  pure $ [(p, p == myAddr) | p <- proposers]

permuteTopic :: Sendable a => a -> Consensus Topic
permuteTopic key = do
  tv <- asks $ mtopic . has
  atomically do
    cur <- readTVar tv <&> getMsg
    let r = hashMsg $ toStrict $ Bin.encode (key, cur)
    writeTVar tv r
    pure r

-- Check Majority -------------------------------------------------------------


collectMajority :: Sendable a => Collect a
collectMajority inv = do
  ConsensusParams{..} <- asks has
  pure $ length inv >= majorityThreshold (length members')

collectThreshold :: Sendable a => Int -> Collect a
collectThreshold n inv = pure $ length inv >= n

collectMembers :: Sendable a => [Address] -> Collect a
collectMembers addrs inv = pure $ all (flip Map.member inv) addrs

majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1

timeDelta :: MonadIO m => UTCTime -> m Int
timeDelta t = f <$> liftIO getCurrentTime where
  f now = round . (* 1000000) . realToFrac $ diffUTCTime now t
