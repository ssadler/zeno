{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Zeno.Consensus.Round
  ( step
  , propose
  , incStep
  , collectMembers
  , collectMajority
  , collectThreshold
  , collectWith
  , runConsensus
  , inventoryIndex
  , prioritiseRemoteInventory
  , dedupeInventoryQueries
  , majorityThreshold
  , spawnChildRound
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
import           UnliftIO.Async (waitCatchSTM, waitSTM)


-- Run round ------------------------------------------------------------------

runConsensus :: (Serialize a, Has ConsensusNode r)
             => String -> ConsensusParams -> a -> Consensus b -> Zeno r b
runConsensus label ccParams@ConsensusParams{..} entropy act = do

  ccStepNum <- newTVarIO (0, Nothing)
  ccChildren <- newTVarIO []
  let ccEntropy = sha3b $ encode entropy
      toCC r = let ccNode = has r in ConsensusContext{..}

  withContext toCC do
    roundId <- getRoundId
    let roundName = "Round %s (%s)" % (roundId, label)
  
    Process{..} <-
      spawn roundName $ \handoff -> do

        withUIProc (UIRound label roundId) do

          handleTimeout roundName do     -- We will re-throw this outside but we
                                         -- don't want it being logged

            act >>= send handoff         -- Send result into handoff so runConsensus can return

            threadDelay $ 10 * 1000000   -- for stragglers to catch up

            pure murphyNoResult          -- This will not get evaluated unless
                                         -- nothing is send to the handoff

    r <- atomically $ orElse (receiveSTM procMbox) (waitSTM procAsync >>= throwSTM)
    logInfo $ "Finished: " ++ roundName
    pure r

  where
  murphyNoResult = murphy "Round finished without a result - this should not happen"
  handleTimeout roundName = do
    handle \c@(ConsensusTimeout _) -> do
      logInfo $ "Timeout: " ++ roundName
      sendUI (UI_Step "Timeout")
      threadDelayS 4
      pure c


spawnChildRound :: Serialize a
                => String -> ConsensusParams -> a -> Consensus () -> Consensus ()
spawnChildRound label ccParams entropy act = do

  -- TODO: when you have to do this lens has become a neccesary evil
  localZeno filterLogWarn do
    proc <- spawn label \_ -> runConsensus label ccParams entropy act
    ConsensusContext{..} <- ask
    atomically $ modifyTVar ccChildren (proc:)
  where
  filterLogWarn app =
    let Console _ status doEvents = appConsole app
     in app { appConsole = Console LevelWarn status False }



-- Coordinate Round -----------------------------------------------------------

type BallotData a = (Typeable a, Serialize a)

-- | Step initiates the procedure of exchanging signed messages.
-- | The step is run in a separate thread, and this thread will wait
-- | until the collect condition has been fulfilled.
step :: BallotData a => String -> Collect a b -> a -> Consensus b
step name collect obj = do
  incStep $ "collect " ++ name
  step' name collect obj

step' :: forall a b. BallotData a => String -> Collect a b -> a -> Consensus b
step' name collect obj = do
  ConsensusParams{ident' = EthIdent sk myAddr, ..} <- asks ccParams
  let sig = sign sk $ toMessage obj
      ballot = Ballot myAddr sig obj
      errTimeout = ConsensusTimeout ("Timeout after %i seconds" % quot timeout' 1000000)

  recv <- spawnStep ballot

  -- The flow is inverted here, we use Left to jump out early
  r <- runExceptT do
    receiveDuring recv timeout' \inv -> do
      pass <- lift $ collect inv
      maybe (pure ()) throwError pass

  case r of
    Left inv -> pure inv
    Right () -> throwIO errTimeout 
  where

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
  determineProposers >>= go True
    where
      go :: Bool -> [(Address, Bool)] -> Consensus (Ballot a)
      go _ [] = throwIO $ ConsensusTimeout "Ran out of proposers"
      go primary ((pAddr, isMe):xs) = do

        major <- incStepNum
        minor <- incMinorStepNum

        obj <-
          if isMe
             then logDebug ("Proposer is: %s (me)" % show pAddr) >> (Just <$> mObj)
             else logDebug ("Proposer is: %s" % show pAddr) >> pure Nothing

        let
          nextProposer (ConsensusTimeout _) = do
            logInfo $ "Timeout collecting for proposer: " ++ show pAddr
            evtProposerTimeout primary pAddr 
            go False xs

          name' = printf "propose %s" name

          collect _ inv =
            case Map.lookup pAddr inv of
              Just (s, Just obj2) -> do
                Just $ Ballot pAddr s obj2
              Nothing -> Nothing

        handle nextProposer do
          step' name' (collectWith collect) obj


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

evtProposerTimeout :: Bool -> Address -> Consensus ()
evtProposerTimeout isPrimary proposer = do
  timeout <- ProposerTimeout proposer <$> getRoundId <*> getStepNum
  ConsensusParams{..} <- asks has
  (maybe mempty id onProposerTimeout') isPrimary timeout

-- Check Majority -------------------------------------------------------------

debugCollect :: Bool
debugCollect = False

collectMajority :: Serialize a => Collect a (Inventory a)
collectMajority inv = do
  ConsensusParams{..} <- asks has
  let have = length inv
      majority = majorityThreshold $ length members'
  pure
    if have >= majority then Just inv else Nothing
  -- when debugCollect do
  --   logDebug $ "collect majority: %i >= %i == %s" % (have, majority, show pass)

-- | collectThreshold collects at least n ballots.
--   At least, because it collects the greater of the given n and
--   the majority threshold.
collectThreshold :: Serialize a => Int -> Collect a (Inventory a)
collectThreshold n inv = do
  ConsensusParams{..} <- asks has
  let t = max n $ majorityThreshold $ length members'
  pure
    if length inv >= t then Just inv else Nothing
  -- when debugCollect do
  --   logDebug $ "collect threshold: %i >= %i == %s" % (length inv, t, show pass)

collectMembers :: Serialize a => [Address] -> Collect a (Inventory a)
collectMembers addrs inv = do
  let pass = all (flip Map.member inv) addrs
  -- when debugCollect do
  --   logDebug $ "collect members: %s ⊆  %s == %s" % (show addrs, show $ Map.keys inv, show pass)
  pure
    if pass then Just inv else Nothing

collectWith :: Serialize a => (Int -> Inventory a -> Maybe b) -> Collect a b
collectWith f inv = do
  ConsensusParams{..} <- asks has
  let majority = majorityThreshold $ length members'
  pure $ f majority inv



majorityThreshold :: Int -> Int
majorityThreshold m = floor $ (fromIntegral m) / 2 + 1
