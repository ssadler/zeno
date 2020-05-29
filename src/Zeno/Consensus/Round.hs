{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zeno.Consensus.Round
  ( ConsensusException(..)
  , step
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
import qualified Data.Map as Map
import           Data.Serialize
import           Data.Time.Clock

import           Network.Ethereum.Crypto
import           Zeno.Process

import           Zeno.Prelude
import           Zeno.Consensus.P2P
import           Zeno.Consensus.Step
import           Zeno.Consensus.Types

import           UnliftIO.STM
import           Control.Monad.STM (orElse, throwSTM)
import           UnliftIO.Async (waitCatchSTM)


-- Run round ------------------------------------------------------------------

runConsensus :: (Serialize a, Has ConsensusNode r) => ConsensusParams
             -> a -> Consensus b -> Zeno r b
runConsensus params@ConsensusParams{..} entropy act = do
  let roundId = toFixed $ sha3' $ encode entropy
  cn <- asks has
  tStepNum <- newTVarIO 0
  let ctx = ConsensusContext cn params roundId tStepNum

  -- TODO: mask here?
  Process{..} <-
    spawn "Consensus Round" $ \proc -> do
      withContext (\_ -> ctx) act >>= send proc
      -- Keep child steps alive for a while, give the stragglers a chance to catch up.
      threadDelay $ 10 * 1000000

  -- TODO: Something funny (but harmless) could happen here if the parent thread is
  -- killed, ie, we could re-throw it from here.
  -- Solution: child thread should post a concensus timeout, not throw it.
  atomically do
    orElse
      (receiveSTM procMbox)
      (waitCatchSTM procAsync >>=
        \case Left e -> throwSTM e
              Right () -> error "Round finished without a result - this should not happen")


-- Coordinate Round -----------------------------------------------------------


-- | Step initiates the procedure of exchanging signed messages.
-- | The step is run in a separate thread, and this thread will wait
-- | until the collect condition has been fulfilled.
step :: Sendable a => String -> Collect a -> a -> Consensus (Inventory a)
step name collect obj = do
  incStepNum
  ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
  topic <- getTopic
  let sig = sign sk $ sha3b $ encode (topic, obj)
  let ballot = Ballot myAddr sig obj

  recv <- spawnStep name ballot members'

  -- TODO: it would be nice if this was merged with receiveDuring
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

-- 1. Determine a starting proposer
-- 2. Try to get a proposal from them
-- 3. If there's a timeout, move to the next proposer
propose :: forall a. Sendable a => String -> Consensus a -> Consensus (Ballot a)
propose name mObj = do
  determineProposers >>= go
    where
      go :: [(Address, Bool)] -> Consensus (Ballot a)
      go [] = throwIO $ ConsensusTimeout "Ran out of proposers"
      go ((pAddr, isMe):xs) = do

        incStepNum

        let nextProposer (ConsensusTimeout _) = do
              logInfo $ "Timeout collecting for proposer: " ++ show pAddr
              go xs
            nextProposer e = throwIO e

        obj <-
          if isMe
             then logInfo ("Proposer is: %s (me)" % show pAddr) >> (Just <$> mObj)
             else logInfo ("Proposer is: %s" % show pAddr) >> pure Nothing

        handle nextProposer $ do
          withTimeout (5 * 1000000) do
            name' <- printf "%s (%i)" name <$> getStepNum
            results <- step name' (collectMembers [pAddr]) obj
            case Map.lookup pAddr results of
                 Just (s, Just obj2) -> do
                   pure $ Ballot pAddr s obj2
                 Just (_, Nothing)   -> do
                   throwIO (ConsensusMischief $ printf "Missing proposal from %s" $ show pAddr)
                 Nothing -> error "Missing proposal; should not happen"

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
  ConsensusContext{ccParams = ConsensusParams{..}, ..} <- ask
  n <- getStepNum
  let msg2sum = sum . map fromIntegral . BS.unpack . sha3' . encode
  let i = mod (msg2sum (ccRoundId, n)) (length members')
      proposers = take 3 $ drop i $ cycle members'
  pure $ [(p, p == undefined "myAddr") | p <- proposers]

-- Check Majority -------------------------------------------------------------

debugCollect :: Bool
debugCollect = False

collectMajority :: Sendable a => Collect a
collectMajority inv = do
  ConsensusParams{..} <- asks has
  let have = length inv
      majority = majorityThreshold $ length members'
      pass = have >= majority
  when debugCollect do
    logDebug $ "collect majority: %i >= %i == %s" % (have, majority, show pass)
  pure pass

-- | collectThreshold collects at least n ballots.
--   At least, because it collects the greater of the given n and
--   the majority threshold.
collectThreshold :: Sendable a => Int -> Collect a
collectThreshold n inv = do
  ConsensusParams{..} <- asks has
  let t = max n $ majorityThreshold $ length members'
  let pass = length inv >= t
  when debugCollect do
    logDebug $ "collect threshold: %i >= %i == %s" % (length inv, t, show pass)
  pure pass

collectMembers :: Sendable a => [Address] -> Collect a
collectMembers addrs inv = do
  let pass = all (flip Map.member inv) addrs
  when debugCollect do
    logDebug $ "collect members: %s ⊆  %s == %s" % (show addrs, show $ Map.keys inv, show pass)
  pure pass

majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
