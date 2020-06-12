{-# LANGUAGE DeriveFunctor #-}

-- | Zeno uses a consensus algorithm, but it is stateless, it doesn't write any data
--   to disk. So, at any point it can pick up the current state from external blockchains
--   without syncing blocks. The "step" is the process whereby the chains are examined
--   and it decides what to do next.

module Zeno.Notariser.Step where

import Control.Monad.Trans.Free.Church
import Control.Monad.Free.TH

import Network.Komodo

import Zeno.Prelude
import Zeno.Notariser.Types
import Zeno.Consensus.Types

import Zeno.EthGateway


--------------------------------------------------------------------------------
-- Notariser interface
--------------------------------------------------------------------------------

type NotariserStep m = FT NotariserStepF m

instance MonadLogger m => MonadLogger (NotariserStep m)

data NotariserStepF next
  = GetLastNotarisationFree     (Maybe (EthNotarisationData, ProposerSequence) -> next)
  | GetLastNotarisationReceipt  (Maybe KomodoNotarisationReceipt -> next)
  | MakeNotarisationReceipt     EthNotarisationData (KomodoNotarisationReceipt -> next)
  | RunNotarise                 ProposerSequence Word32 (() -> next)
  | RunNotariseReceipt          ProposerSequence KomodoNotarisationReceipt (() -> next)
  | WaitSourceHeightFree        Word32 (Word32 -> next)
  deriving (Functor)

makeFree ''NotariserStepF

data Done = Done                -- This little guy helps disambiguate between
  deriving (Eq, Show)           -- termination and early return when testing


--------------------------------------------------------------------------------
-- Step algorithm
--------------------------------------------------------------------------------

notariserStepFree :: forall m. MonadLogger m => NotariserConfig -> NotariserStep m Done
notariserStepFree nc@NotariserConfig{..} = do
  getLastNotarisationFree >>= go >> pure Done
  where
  go :: Maybe (EthNotarisationData, ProposerSequence) -> NotariserStep m ()
  go Nothing = do
    logInfo "No prior notarisations found"
    forward 0 0

  go (Just (notarisation, sequence)) = do

    let sourceHeight = foreignHeight notarisation

    logDebug $ "Found notarisation on ETH for %s.%i" %
               (kmdChainSymbol, sourceHeight)

    getLastNotarisationReceipt >>=
      \case
        Just receipt
          | sourceHeight == notarisedHeight receipt -> do
            logDebug "Found backnotarisation, proceed with next notarisation"
            forward sequence sourceHeight
          | sourceHeight == notarisedHeight receipt -> do
            logError $ show notarisation
            logError $ show receipt
            logError "The backnotarised height in KMD is higher than the notarised\
                     \ height in ETH. Is ETH node is lagging? Proceeding anyway."
            forward sequence sourceHeight
 
        _ -> do
          logDebug "Backnotarisation not found, proceed to backnotarise"
          opret <- makeNotarisationReceipt notarisation
          let seq = shiftSequence 2 sequence
          runNotariseReceipt seq opret

  forward :: ProposerSequence -> Word32 -> NotariserStep m ()
  forward sequence sourceHeight = do
      newHeight <- waitSourceHeightFree sourceHeight
      runNotarise sequence newHeight

  shiftSequence n seq =
    seq + ProposerSequence (quot (length members) n)
