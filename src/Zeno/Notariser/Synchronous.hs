{-# LANGUAGE KindSignatures #-}

-- | Zeno uses a consensus algorithm, but it is stateless, it doesn't write any data
--   to disk. So, at any point it can pick up the current state from external blockchains
--   without syncing blocks. The synchronous process performs notarisations back and forth
--   between two chains in a synchronous manner.

module Zeno.Notariser.Synchronous where

import Data.Bits

import Control.Monad.Skeleton

import Network.Komodo

import Zeno.Prelude
import Zeno.Notariser.Types
import Zeno.Notariser.Targets
import Zeno.Consensus.Types

import Zeno.EthGateway


--------------------------------------------------------------------------------
-- Synchrnous Notariser interface
--------------------------------------------------------------------------------

type ProposerSequence = Int

type NotariserSync s d m = Skeleton (NotariserSyncI s d m)

instance MonadLogger m => MonadLogger (NotariserSync s d m) where
  monadLoggerLog a b c d = bone $ NotariserSyncLift $ monadLoggerLog a b c d

instance MonadIO m => MonadIO (Skeleton (NotariserSyncI a b m)) where
  liftIO = bone . NotariserSyncLift . liftIO

data NotariserSyncI s d m x where
  GetLastNotarisation         :: NotariserSyncI s d m (Maybe (ChainNotarisation d, ProposerSequence))
  GetLastNotarisationReceipt  :: NotariserSyncI s d m (Maybe (ChainNotarisationReceipt s))
  RunNotarise                 :: ProposerSequence -> Word32 -> Maybe (ChainNotarisationReceipt s) -> NotariserSyncI s d m ()
  RunNotariseReceipt          :: ProposerSequence -> Word32 -> ChainNotarisation d -> NotariserSyncI s d m ()
  WaitNextSourceHeight        :: Word32 -> NotariserSyncI s d m (Maybe Word32)
  WaitNextDestHeight          :: Word32 -> NotariserSyncI s d m (Maybe Word32)
  NotariserSyncLift           :: m a -> NotariserSyncI s d m a

--------------------------------------------------------------------------------
-- Synchrnous Notariser skeleton
--------------------------------------------------------------------------------

type Notariser a b m = (SourceChain a m, DestChain b m, MonadLogger m)

notariserSyncFree :: forall a b m. Notariser a b m
                  => AbstractNotariserConfig a b -> Skeleton (NotariserSyncI a b m) ()
notariserSyncFree nc@NotariserConfig{..} = start
  where
  start = bone GetLastNotarisation >>= go

  go :: Maybe (ChainNotarisation b, ProposerSequence) -> NotariserSync a b m ()
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

    bone GetLastNotarisationReceipt >>=
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
