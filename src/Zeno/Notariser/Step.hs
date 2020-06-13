{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}

-- | Zeno uses a consensus algorithm, but it is stateless, it doesn't write any data
--   to disk. So, at any point it can pick up the current state from external blockchains
--   without syncing blocks. The "step" is the process whereby the chains are examined
--   and it decides what to do next.

module Zeno.Notariser.Step where

import Data.Bits

import Control.Monad.Trans.Free.Church
import Control.Monad.Free.TH

import Network.Komodo

import Zeno.Prelude
import Zeno.Notariser.Types
import Zeno.Notariser.Targets
import Zeno.Consensus.Types

import Zeno.EthGateway


--------------------------------------------------------------------------------
-- Notariser interface
--------------------------------------------------------------------------------

type NotariserStep a b m = FT (NotariserStepF a b m) m

instance MonadLogger m => MonadLogger (NotariserStep a b m)

type ProposerSequence = Int

data NotariserStepF source dest (m :: * -> *) next
  = GetLastNotarisationFree         (Maybe (ChainNotarisation dest, ProposerSequence) -> next)
  | GetLastNotarisationReceiptFree  (Maybe (ChainNotarisationReceipt source) -> next)
  | MakeNotarisationReceipt         (ChainNotarisation dest) (ChainNotarisationReceipt source -> next)
  | RunNotarise                     ProposerSequence Word32 (() -> next)
  | RunNotariseReceipt              ProposerSequence (ChainNotarisationReceipt source) (() -> next)
  | WaitNextSourceHeight            Word32 (Maybe Word32 -> next)
  | WaitNextDestHeight              Word32 (Maybe Word32 -> next)

deriving instance (Functor (NotariserStepF a b m))

makeFree ''NotariserStepF

data Done = Done                -- This little guy helps disambiguate between
  deriving (Eq, Show)           -- termination and early return when testing

--------------------------------------------------------------------------------
-- Step algorithm
--------------------------------------------------------------------------------

type Notariser a b m = (MonadLogger m, SourceChain a m, DestChain b m)

notariserStepFree :: forall a b m. Notariser a b m
                  => AbstractNotariserConfig a b -> NotariserStep a b m Done
notariserStepFree nc@NotariserConfig{..} = start >> pure Done
  where

  go :: Maybe (ChainNotarisation b, ProposerSequence) -> NotariserStep a b m ()
  go Nothing = do
    logInfo "No prior notarisations found"
    forward 0 0

  go (Just (notarisation, sequence)) = do

    let notarisedHeight = foreignHeight notarisation

    logDebug $ "Found notarisation on %s for %s.%i" %
               (getSymbol destChain, getSymbol sourceChain, notarisedHeight)

    getLastNotarisationReceiptFree >>=
      \case
        Just receipt
          | notarisedHeight <= receiptHeight receipt -> do

            if notarisedHeight == receiptHeight receipt
               then do
                 logDebug "Found receipt, proceed with next notarisation"
               else do
                 logError $ show notarisation
                 logError $ show receipt
                 logError $ "The receipt height in %s is higher than the notarised\
                            \ height in %s. Is %s node is lagging? Proceeding anyway." %
                            (getSymbol sourceChain, getSymbol destChain, getSymbol destChain)

            forward sequence notarisedHeight
 
        _ -> do
          logDebug "Receipt not found, proceed to backnotarise"
          opret <- makeNotarisationReceipt notarisation
          let seq = sequence + quot (length members) 2
          runNotariseReceipt seq opret

  start = getLastNotarisationFree >>= go

  forward sequence lastHeight = do
    waitNextSourceHeight lastHeight >>=
      \case Nothing -> start
            Just h -> runNotarise sequence h


-- | This function has been thoroughly eyeballed
waitNextNotariseHeight :: (MonadIO m, MonadLogger m, BlockchainAPI c m) => c -> Word32 -> m (Maybe Word32)
waitNextNotariseHeight chain lastHeight = do
  height <- getHeight chain
  let interval = getNotarisationBlockInterval chain
  let nextHeight = getNextHeight interval lastHeight height
  if (height >= nextHeight)
     then pure $ Just nextHeight
     else do
       Nothing <$ do
          logInfo $ "Waiting for %s height: %i" % (getSymbol chain, nextHeight)
          fix \f -> do
            threadDelayS 5
            height <- getHeight chain
            when (height < nextHeight) f


getNextHeight :: Word32 -> Word32 -> Word32 -> Word32
getNextHeight interval last current =
  let nextLo = last + interval - mod last interval
      nextHi = current - mod current interval
   in max nextLo nextHi
