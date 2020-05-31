{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zeno.Consensus.Round
  ( ConsensusException(..)
  , step
  , propose
  , incStep
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
import           Data.Typeable
import           Unsafe.Coerce

import           Network.Ethereum.Crypto
import           Zeno.Process

import           Zeno.Prelude
import           Zeno.Consensus.P2P
import           Zeno.Consensus.Step
import           Zeno.Consensus.Types
import           Zeno.Console

import           UnliftIO.STM
import           Control.Monad.STM (orElse, throwSTM)
import           UnliftIO.Async (waitCatchSTM)


-- Run round ------------------------------------------------------------------

runConsensus :: (Serialize a, Has ConsensusNode r)
             => String -> ConsensusParams -> a -> Consensus b -> Zeno r b
runConsensus label params@ConsensusParams{..} entropy act = do
  cn <- asks has
  tStepNum <- newTVarIO (0, Nothing)
  let ctx = ConsensusContext cn params (sha3b $ encode entropy) tStepNum

  -- TODO: mask here?
  Process{..} <-
    spawn "Consensus Round" $ \proc -> do
      withContext (\_ -> ctx) actWrapped >>= send proc
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

  where
  actWrapped = do
    roundId <- getRoundId
    logInfo $ "New round: %s (%s)" % (roundId, label)
    withUIProc (UIRound label roundId) act


-- Coordinate Round -----------------------------------------------------------

type BallotData a = (Typeable a, Serialize a)

-- | Step initiates the procedure of exchanging signed messages.
-- | The step is run in a separate thread, and this thread will wait
-- | until the collect condition has been fulfilled.
step :: BallotData a => String -> Collect a -> a -> Consensus [Ballot a]
step name collect obj = do
  incStep $ "collect " ++ name
  unInventory <$> step' name collect obj

step' :: forall a. BallotData a => String -> Collect a -> a -> Consensus (Inventory a)
step' name collect obj = do
  ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
  let sig = sign sk $ toMessage obj
  let ballot = Ballot myAddr sig obj

  recv <- spawnStep ballot

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

  toMessage =
    if typeRep (Proxy :: Proxy a) == typeRep (Proxy :: Proxy Bytes32)
       then unsafeCoerce
       else sha3b . encode

incStep :: String -> Consensus ()
incStep label = do
  major <- incStepNum
  sendUI $ UI_Step $ "%i: %s" % (major, label)


-- 1. Determine a starting proposer
-- 2. Try to get a proposal from them
-- 3. If there's a timeout, move to the next proposer
-- TODO: Non proposers should not send any message until timeout.
-- That way, a node playing catchup can detect a timeout by listening
-- for a majority of timeout votes.
propose :: forall a. BallotData a => String -> Consensus a -> Consensus (Ballot a)
propose name mObj = do
  determineProposers >>= go
    where
      go :: [(Address, Bool)] -> Consensus (Ballot a)
      go [] = throwIO $ ConsensusTimeout "Ran out of proposers"
      go ((pAddr, isMe):xs) = do

        major <- incStepNum
        minor <- incMinorStepNum

        let nextProposer (ConsensusTimeout _) = do
              logInfo $ "Timeout collecting for proposer: " ++ show pAddr
              go xs
            nextProposer e = throwIO e

        obj <-
          if isMe
             then logDebug ("Proposer is: %s (me)" % show pAddr) >> (Just <$> mObj)
             else logDebug ("Proposer is: %s" % show pAddr) >> pure Nothing

        handle nextProposer $ do
          withTimeout (5 * 1000000) do
            let name' = printf "propose %s" name
            results <- step' name' (collectMembers [pAddr]) obj
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
  roundId <- getRoundId
  let msg2sum = sum . map fromIntegral . BS.unpack . sha3' . encode
  let i = mod (msg2sum (roundId, n)) (length members')
      proposers = take 3 $ drop i $ cycle members'
  pure $ [(p, p == ethAddress ident') | p <- proposers]

-- Check Majority -------------------------------------------------------------

debugCollect :: Bool
debugCollect = False

collectMajority :: Serialize a => Collect a
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
collectThreshold :: Serialize a => Int -> Collect a
collectThreshold n inv = do
  ConsensusParams{..} <- asks has
  let t = max n $ majorityThreshold $ length members'
  let pass = length inv >= t
  when debugCollect do
    logDebug $ "collect threshold: %i >= %i == %s" % (length inv, t, show pass)
  pure pass

collectMembers :: Serialize a => [Address] -> Collect a
collectMembers addrs inv = do
  let pass = all (flip Map.member inv) addrs
  when debugCollect do
    logDebug $ "collect members: %s ⊆  %s == %s" % (show addrs, show $ Map.keys inv, show pass)
  pure pass

majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
