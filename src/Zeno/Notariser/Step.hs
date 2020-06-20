{-# LANGUAGE KindSignatures #-}

-- | Zeno uses a consensus algorithm, but it is stateless, it doesn't write any data
--   to disk. So, at any point it can pick up the current state from external blockchains
--   without syncing blocks. The "step" is the process whereby the chains are examined
--   and it decides what to do next.

module Zeno.Notariser.Step where

import Data.Bits

import Control.Monad.Skeleton

import Network.Komodo

import Zeno.Prelude
import Zeno.Notariser.Types
import Zeno.Notariser.Targets
import Zeno.Consensus.Types

import Zeno.EthGateway


--------------------------------------------------------------------------------
-- Notariser interface
--------------------------------------------------------------------------------


type ProposerSequence = Int

type NotariserStep s d m = Skeleton (NotariserStepI s d m)

instance MonadLogger m => MonadLogger (NotariserStep s d m) where
  monadLoggerLog a b c d = bone $ NotariserStepLift $ monadLoggerLog a b c d

instance MonadIO m => MonadIO (Skeleton (NotariserStepI a b m)) where
  liftIO = bone . NotariserStepLift . liftIO

data NotariserStepI s d (m :: * -> *) x where
  GetLastNotarisationFree         :: NotariserStepI s d m (Maybe (ChainNotarisation d, ProposerSequence))
  GetLastNotarisationReceiptFree  :: NotariserStepI s d m (Maybe (ChainNotarisationReceipt s))
  RunNotarise                     :: ProposerSequence -> Word32 -> Maybe (ChainNotarisationReceipt s) -> NotariserStepI s d m ()
  RunNotariseReceipt              :: ProposerSequence -> Word32 -> ChainNotarisation d -> NotariserStepI s d m ()
  WaitNextSourceHeight            :: Word32 -> NotariserStepI s d m (Maybe Word32)
  WaitNextDestHeight              :: Word32 -> NotariserStepI s d m (Maybe Word32)
  NotariserStepLift               :: m a -> NotariserStepI s d m a

--------------------------------------------------------------------------------
-- Step algorithm
--------------------------------------------------------------------------------

type Notariser a b m = (SourceChain a m, DestChain b m, MonadLogger m)

notariserStepFree :: forall a b m. Notariser a b m
                  => AbstractNotariserConfig a b -> Skeleton (NotariserStepI a b m) ()
notariserStepFree nc@NotariserConfig{..} = start
  where
  start = bone GetLastNotarisationFree >>= go

  go :: Maybe (ChainNotarisation b, ProposerSequence) -> NotariserStep a b m ()
  go Nothing = do
    logInfo "No prior notarisations found"
    notarise 0 0 Nothing

  go (Just (notarisation, sequence)) = do

    let
      notarisedHeight = foreignHeight notarisation

      backnotarise lastHeight = do
        bone (WaitNextDestHeight lastHeight) >>=
          \case
            Nothing -> start
            Just newHeight -> do
              let seq = sequence + quot (length members) 2
              bone $ RunNotariseReceipt seq newHeight notarisation

    logDebug $ "Found notarisation on %s for %s.%i" %
               (getSymbol destChain, getSymbol sourceChain, notarisedHeight)

    bone GetLastNotarisationReceiptFree >>=
      \case
        Just receipt -> do
          let fwd = notarise sequence notarisedHeight (Just receipt)
          case compare (receiptHeight receipt) notarisedHeight of
            LT -> do
              logDebug $ "Posting receipt to %s" % getSymbol sourceChain
              backnotarise (foreignHeight receipt)
            EQ -> fwd <* do
              logDebug $ "Found receipt for %s.%i on %s, proceed with next notarisation" %
                         (getSymbol sourceChain, notarisedHeight, getSymbol destChain)
            GT  -> fwd <* do
              logError $ show notarisation
              logError $ show receipt
              logError $ "The receipt height in %s is higher than the notarised\
                         \ height in %s. Is %s node is lagging? Proceeding anyway." %
                         (getSymbol sourceChain, getSymbol destChain, getSymbol destChain)
 
        _ -> do
          logDebug "Receipt not found, proceed to backnotarise"
          backnotarise 0

  notarise sequence lastHeight mlastReceipt = do
    bone (WaitNextSourceHeight lastHeight) >>=
      \case Nothing -> start
            Just h -> bone $ RunNotarise sequence h mlastReceipt


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
