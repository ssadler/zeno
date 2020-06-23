{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module TestNotariserSynchronous where

import TestUtils

import Control.Monad.Except
import Control.Monad.Identity
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

type TestBase = IO
runTestBase = id
instance MonadLogger Identity where monadLoggerLog a b c d = pure ()
instance MonadLogger IO where monadLoggerLog a b c d = pure ()


data TestChain = TestChain { unTestChain :: String }
instance BlockchainConfig TestChain where
  getSymbol = unTestChain
  getNotarisationBlockInterval _ = 5

instance BlockchainAPI TestChain TestBase where
  getHeight = error "testchainwaitheight"

instance SourceChain TestChain TestBase where
  type (ChainNotarisationReceipt TestChain) = (Word32, Word32)
  getLastNotarisationReceipt = error "getLastNotarisationReceipt"

instance DestChain TestChain TestBase where
  type (ChainNotarisation TestChain) = Word32
  getLastNotarisationAndSequence = error "getLastNotarisationAndSequence"

instance Notarisation Word32 where
  foreignHeight = id

instance Notarisation (Word32, Word32) where
  foreignHeight = snd

instance NotarisationReceipt (Word32, Word32) where
  receiptHeight = fst
 


type Bone = NotariserSyncI TestChain TestChain TestBase


runStep :: Maybe (ChainNotarisation TestChain, ProposerSequence)
        -> Maybe (ChainNotarisationReceipt TestChain)
        -> MonadView Bone (Skeleton Bone) a -> TestBase (Skeleton Bone a)
runStep mdest msource =
  \case
    GetLastNotarisation             :>>= f -> pure $ f mdest
    GetLastNotarisationReceipt      :>>= f -> pure $ f msource
    WaitNextSourceHeight lastHeight     :>>= f -> pure $ f $ Just $ lastHeight + 1
    WaitNextDestHeight   lastHeight     :>>= f -> pure $ f $ Just $ lastHeight + 1
    NotariserSyncLift act               :>>= f -> f <$> act
    RunNotarise last current mreceipt   :>>= f -> error "exited"
    RunNotariseReceipt seq bnd lastNota :>>= f -> error "exited"

members' = Address . newFixed <$> [1..42]
nc = NotariserConfig members' (error "threhsold") (error "timeout") (error "proposerRoundRobin") (TestChain "SRC") (TestChain "DEST")
go :: (MonadView Bone (Skeleton Bone) () -> TestBase (Skeleton Bone ())) -> TestBase ()
go f = inner $ debone $ notariserSyncFree nc
  where
    inner (Return ()) = pure ()
    inner o = f o >>= inner . debone

next :: Skeleton Bone a -> TestBase (MonadView Bone (Skeleton Bone) a)
next skel =
  case debone skel of
    NotariserSyncLift act :>>= f -> act >>= next . f
    o -> pure o


test_notariser_step :: TestTree
test_notariser_step = testGroup "notarises"
  [
    testCase "forward when there are no notarisations" do

      GetLastNotarisation :>>= f <- next $ notariserSyncFree nc
      WaitNextSourceHeight nextHeight :>>= f <- next $ f Nothing
      nextHeight `shouldBe` 0
      RunNotarise seq h Nothing :>>= f <- next $ f $ Just 20
      seq `shouldBe` 0
      h `shouldBe` 20
      Return () <- next $ f ()
      pure ()

  , testCase "backward when there is no receipt" do
      go \case
        RunNotariseReceipt _ thisHeight ndata :>>= f -> do
          liftIO $ ndata `shouldBe` (1 :: Word32)
          pure $ f ()
        o -> runStep (Just (1, 98)) Nothing o

  , testCase "backward when there is a lower receipt" do
      go \case
        RunNotariseReceipt _ thisHeight ndata :>>= f -> do
          ndata `shouldBe` 75
          pure $ f ()
        o -> runStep (Just (75, 98)) (Just (74, 0)) o
        
  , testCase "forward when there is an equal receipt" do
      go \case
        WaitNextSourceHeight nextHeight :>>= f -> do
          nextHeight `shouldBe` 75
          pure $ f $ Just 80
        RunNotarise seq h (Just _) :>>= f -> do
          h `shouldBe` 80
          pure $ f ()
        o -> runStep (Just (75, 98)) (Just (75, 0)) o

  , testCase "repeat when wa, testCase for source block" do
      GetLastNotarisation :>>= f <- next $ notariserSyncFree nc
      WaitNextSourceHeight nextHeight :>>= f <- next $ f Nothing
      nextHeight `shouldBe` 0
      GetLastNotarisation :>>= f <- next $ f Nothing
      pure ()

  , testCase "repeat when wa, testCase for dest block" do
      GetLastNotarisation :>>= f <- next $ notariserSyncFree nc
      GetLastNotarisationReceipt :>>= f <- next $ f (Just (10, 1))
      WaitNextDestHeight nextHeight :>>= f <- next $ f (Just (1, 1))
      nextHeight `shouldBe` 1
      GetLastNotarisation :>>= f <- next $ f Nothing
      pure ()
  ]


test_notariser_proposer_sequence :: TestTree
test_notariser_proposer_sequence = testGroup "proposer sequence"
  [
    testCase "first" do
      go \case
        RunNotarise seq _ _ :>>= f -> do
          seq `shouldBe` 0
          pure $ f ()
        o -> runStep Nothing undefined o

  , testCase "forward" do
      go \case
        RunNotarise seq _ _ :>>= f -> do
          seq `shouldBe` 120
          pure $ f ()
        o -> runStep (Just (75, 120)) (Just (75, 75)) o

  , testCase "back" do
      go \case
        RunNotariseReceipt seq _ _ :>>= f -> do
          seq `shouldBe` 141
          pure $ f ()
        o -> runStep (Just (10, 120)) Nothing o
  ]
