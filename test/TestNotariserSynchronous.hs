{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TestNotariserSynchronous where

import TestUtils

import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Skeleton

import Data.Word
import Data.FixedBytes
import Data.Void

import Network.Ethereum (Address(..))

import Zeno.Notariser.Types
import Zeno.Notariser.Targets
import Zeno.Notariser.Synchronous
import Zeno.Prelude


data TestChain = TestChain { unTestChain :: String }
instance BlockchainConfig TestChain where
  getSymbol = unTestChain
  getNotarisationBlockInterval _ = 5

instance BlockchainAPI TestChain TestIO where
  getHeight = error "testchainwaitheight"

instance SourceChain TestChain TestIO where
  type (ChainNotarisationReceipt TestChain) = (Word32, Word32)
  getLastNotarisationReceipt = error "getLastNotarisationReceipt"

instance DestChain TestChain TestIO where
  type (ChainNotarisation TestChain) = Word32
  getLastNotarisationAndSequence = error "getLastNotarisationAndSequence"

instance Notarisation Word32 where
  foreignHeight = id

instance Notarisation (Word32, Word32) where
  foreignHeight = snd

instance NotarisationReceipt (Word32, Word32) where
  receiptHeight = fst
 


type Bone = NotariserSyncI TestChain TestChain TestIO


runStep :: Maybe (ChainNotarisation TestChain)
        -> Maybe (ChainNotarisationReceipt TestChain)
        -> MonadView Bone (Skeleton Bone) a -> TestIO (Skeleton Bone a)
runStep mdest msource =
  \case
    GetLastNotarisation             :>>= f -> pure $ f mdest
    GetLastNotarisationReceipt      :>>= f -> pure $ f msource
    WaitNextSourceHeight lastHeight :>>= f -> pure $ f $ Just $ lastHeight + 1
    WaitNextDestHeight   lastHeight :>>= f -> pure $ f $ Just $ lastHeight + 1
    NotariserSyncLift act           :>>= f -> f <$> act
    RunNotarise current mreceipt    :>>= f -> error "exited"
    RunNotariseReceipt bnd lastNota :>>= f -> error "exited"

members' = Address . newFixed <$> [1..42]
nc = NotariserConfig members' (error "threhsold") (error "timeout") (TestChain "SRC") (TestChain "DEST")
go :: (MonadView Bone (Skeleton Bone) () -> TestIO (Skeleton Bone ())) -> TestIO ()
go f = inner $ debone $ notariserSyncFree nc
  where
    inner (Return ()) = pure ()
    inner o = f o >>= inner . debone

next :: Skeleton Bone a -> TestIO (MonadView Bone (Skeleton Bone) a)
next skel =
  case debone skel of
    NotariserSyncLift act :>>= f -> act >>= next . f
    o -> pure o


test_notariser_step :: TestTree
test_notariser_step = testGroup "notarises"
  [
    testIOCase "forward when there are no notarisations" do

      GetLastNotarisation :>>= f <- next $ notariserSyncFree nc
      WaitNextSourceHeight nextHeight :>>= f <- next $ f Nothing
      nextHeight @?= 0
      RunNotarise h Nothing :>>= f <- next $ f $ Just 20
      h @?= 20
      Return () <- next $ f ()
      pure ()

  , testIOCase "backward when there is no receipt" do
      go \case
        RunNotariseReceipt thisHeight ndata :>>= f -> do
          liftIO $ ndata @?= (1 :: Word32)
          pure $ f ()
        o -> runStep (Just 1) Nothing o

  , testIOCase "backward when there is a lower receipt" do
      go \case
        RunNotariseReceipt thisHeight ndata :>>= f -> do
          ndata @?= 75
          pure $ f ()
        o -> runStep (Just 75) (Just (74, 0)) o
        
  , testIOCase "forward when there is an equal receipt" do
      go \case
        WaitNextSourceHeight nextHeight :>>= f -> do
          nextHeight @?= 75
          pure $ f $ Just 80
        RunNotarise h (Just _) :>>= f -> do
          h @?= 80
          pure $ f ()
        o -> runStep (Just 75) (Just (75, 0)) o

  , testIOCase "repeat when waited for source block" do
      GetLastNotarisation :>>= f <- next $ notariserSyncFree nc
      WaitNextSourceHeight nextHeight :>>= f <- next $ f Nothing
      nextHeight @?= 0
      GetLastNotarisation :>>= f <- next $ f Nothing
      pure ()

  , testIOCase "repeat when waited for dest block" do
      GetLastNotarisation :>>= f <- next $ notariserSyncFree nc
      GetLastNotarisationReceipt :>>= f <- next $ f (Just 10)
      WaitNextDestHeight nextHeight :>>= f <- next $ f (Just (1, 1))
      nextHeight @?= 1
      GetLastNotarisation :>>= f <- next $ f Nothing
      pure ()
  ]
