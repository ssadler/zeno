{-# LANGUAGE MultiWayIf #-}

module Zeno.Notariser.Collect where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Zeno.Console
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process


collectWeighted :: forall i. [Address] -> Int -> Collect i (Inventory i)
collectWeighted distribution n recv = do

  -- The distribution defines a priority order for UTXO input preference.
  -- It is TBD but it might have a reward system like the following.
  -- The first n of the distribution all have equal weight, and they are
  -- the preferred inputs. Then, for the next n inputs, each is worth 1 point
  -- less than the previous. After that, all have low value. So, graphically:
  --
  --             13          13          38
  --         __________
  --                   \_
  --                     \_
  --                       \_
  --                         \_
  --                           \_____________________________
  --
  --         where n = 13, total = 64
  --
  -- The condition for finishing the collection is that at least n signatures
  -- have been obtained, and a delay is added in order to maximise weight.

  let
    primary = Set.fromList $ take n distribution
    sortDist b = elemIndex (bMember b) distribution <|> murphy "non members in inventory"
    havePrimaries inv = length (Map.restrictKeys inv primary) == n
    timeout1 = 10 * 1000000
    fin = toInventory . take n . sortOn sortDist . unInventory

  -- The delay is short, since we don't know how long the peers have already been
  -- waiting for us to respond. If we knew we were all starting at the same time,
  -- we could comfortably make it longer.

  fin <$> finally
    do
      startTime <- liftIO $ getCurrentTime
      fix1 mempty \f oldInv -> do
        d <- liftIO $ timeDelta startTime

        inv <- receiveTimeout recv 500000 >>=
          \case
            Nothing -> pure oldInv
            Just inv -> do
              sendUI $ UI_MofN (length inv) n
              pure inv

        if
          | havePrimaries inv -> do            -- Have all best sigs, jump out early
              logInfo "Winning!" $> inv 
          | d < timeout1    -> f inv           -- Still within timeout, wait for more
          | length inv >= n -> pure inv        -- Timeout but have enough sigs
          | otherwise -> do                    -- Timeout and don't have enough sigs
              throwIO ConsensusTimeout

    do sendUI $ UI_MofN 0 0

