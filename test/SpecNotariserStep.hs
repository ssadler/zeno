{-# LANGUAGE TypeFamilies #-}

module SpecNotariserStep where

import TestUtils

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Free hiding (iterT)
import Control.Monad.Trans.Free.Church
import Control.Monad.Logger

import Data.Word
import Data.FixedBytes
import Data.Void

import Network.Ethereum (Address(..))

import Zeno.Notariser.Types
import Zeno.Notariser.Targets
import Zeno.Notariser.Step
import Debug.Trace


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
 
runStep mdest msource =
  \case
    GetLastNotarisationFree f             -> f mdest
    WaitNextSourceHeight lastHeight f     -> f $ Just $ lastHeight + 1
    WaitNextDestHeight   lastHeight f     -> f $ Just $ lastHeight + 1
    GetLastNotarisationReceiptFree f      -> f msource
    RunNotarise last current mreceipt f   -> error "exited"
    RunNotariseReceipt seq bnd lastNota f -> error "exited"


spec_notariser_step :: Spec
spec_notariser_step = do

  let
    e = error . show
    members = Address . newFixed <$> [1..42]
    c1 = TestChain "SRC"
    c2 = TestChain "DEST"
    nc = NotariserConfig members (e 2) (e 3) False c1 c2
    next f = runFreeT f >>= \case Pure a -> error "Pure"; Free f -> pure f
    term f = runFreeT f >>= \case Free _ -> error "Free"; Pure a -> pure a
    fin f = term (f ()) >>= (@?= Done)

    go :: (NotariserStepF TestChain TestChain TestBase (TestBase Done) -> TestBase Done) -> TestBase ()
    go f = do
      Done <- runTestBase $ flip iterT (notariserStepFree nc) f
      pure ()

  describe "notarises" do
    it "forward when there are no notarisations" $ do

      GetLastNotarisationFree f <- next $ fromFT (notariserStepFree nc)
      WaitNextSourceHeight nextHeight f <- next $ f Nothing
      nextHeight `shouldBe` 0
      RunNotarise seq h Nothing f <- next $ f $ Just 20
      seq `shouldBe` 0
      h `shouldBe` 20
      fin f

    it "backward when there is no receipt" do
      go \case
        RunNotariseReceipt _ thisHeight ndata f -> do
          ndata `shouldBe` 1
          f ()
        o -> runStep (Just (1, 98)) Nothing o

    it "backward when there is a lower receipt" do
      go \case
        RunNotariseReceipt _ thisHeight ndata f -> do
          ndata `shouldBe` 75
          f ()
        o -> runStep (Just (75, 98)) (Just (74, 0)) o
        
    it "forward when there is an equal receipt" do
      go \case
        WaitNextSourceHeight nextHeight f -> do
          nextHeight `shouldBe` 75
          f $ Just 80
        RunNotarise seq h (Just _) f -> do
          h `shouldBe` 80
          f ()
        o -> runStep (Just (75, 98)) (Just (75, 0)) o

    it "repeat when wait for source block" do
      GetLastNotarisationFree f <- next $ fromFT (notariserStepFree nc)
      WaitNextSourceHeight nextHeight f <- next $ f Nothing
      nextHeight `shouldBe` 0
      GetLastNotarisationFree f <- next $ f Nothing
      pure ()

    it "repeat when wait for dest block" do
      GetLastNotarisationFree f <- next $ fromFT (notariserStepFree nc)
      GetLastNotarisationReceiptFree f <- next $ f (Just (10, 1))
      WaitNextDestHeight nextHeight f <- next $ f (Just (1, 1))
      nextHeight `shouldBe` 1
      GetLastNotarisationFree f <- next $ f Nothing
      pure ()
  
  describe "proposer sequence" do

    it "first" do
      go \case
        RunNotarise seq _ _ f -> do
          seq `shouldBe` 0
          f ()
        o -> runStep Nothing undefined o

    it "forward" do
      go \case
        RunNotarise seq _ _ f -> do
          seq `shouldBe` 120
          f ()
        o -> runStep (Just (75, 120)) (Just (75, 75)) o

    it "back" do
      go \case
        RunNotariseReceipt seq _ _ f -> do
          seq `shouldBe` 141
          f ()
        o -> runStep (Just (10, 120)) Nothing o
