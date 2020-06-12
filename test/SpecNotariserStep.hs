
module SpecNotariserStep where

import TestUtils

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Free.Church
import Control.Monad.Logger

import Data.FixedBytes
import Data.Void

import Network.Ethereum.Crypto
import Network.Komodo

import Zeno.Consensus.Types
import Zeno.Notariser.Types
import Zeno.Notariser.Step
import Zeno.EthGateway
import Debug.Trace


-- type TestBase = NoLoggingT IO
-- 
-- runTestBase :: TestBase a -> IO a
-- runTestBase = runNoLoggingT

type TestBase = IO
runTestBase = id
instance MonadLogger IO where
  monadLoggerLog a b c d = pure ()

noe h = NOE h (error "ethSha3") (error "ethHeight") (error "ethData")

bnd = KomodoNotarisationReceipt nor

nor = NOR
    { norBlockHash = newFixed 0
    , norBlockNumber = 75
    , norForeignRef = newFixed 0xFF
    , norSymbol = "abc"
    , norMom = minBound
    , norMomDepth = minBound
    , norCcId = minBound
    , norMomom = minBound
    , norMomomDepth = minBound
    }
  where
    e :: HasCallStack => e
    e = error "nor"


-- TODO: arbitrary
runStep mlastNota mnoe bnd =
  \case
    GetLastNotarisationFree f         -> f mlastNota
    WaitSourceHeightFree lastHeight f -> f undefined
    GetLastNotarisationReceipt f      -> f mnoe
    MakeNotarisationReceipt noe f     -> f bnd
    RunNotarise last current f        -> error "exited"
    RunNotariseReceipt seq bnd f      -> error "exited"


spec_notariser_step :: Spec
spec_notariser_step = do

  let
    e = error . show
    members = Address . newFixed <$> [1..42]
    nc = NotariserConfig members (e 2) (e 3) (e 4) (e 5) (e 6) (e 7) (e 8) (e 9)

    go :: (NotariserStepF (IO Done) -> IO Done) -> IO ()
    go f = do
      Done <- runTestBase $ flip iterT (notariserStepFree nc) f
      pure ()

  describe "notarises" do
    it "forward when there are no notarisations" do
      go \case
        WaitSourceHeightFree lastHeight f -> do
          lastHeight `shouldBe` 0
          f 20
        RunNotarise seq h f -> do
          seq `shouldBe` 0
          h `shouldBe` 20
          f ()
        o -> runStep Nothing Nothing undefined o

    it "backward when there is no receipt" do
      go \case
        RunNotariseReceipt _ ndata f -> do
          ndata `shouldBe` bnd
          f ()
        o -> runStep (Just (noe 1, 98)) Nothing bnd o

    it "backward when there is a lower receipt" do
      let back = KomodoNotarisationReceipt $ nor { norBlockNumber = 74 }
      go \case
        RunNotariseReceipt _ ndata f -> do
          ndata `shouldBe` bnd
          f ()
        o -> runStep (Just (noe 75, 98)) (Just back) bnd o
        
    it "forward when there is an equal receipt" do
      let back = KomodoNotarisationReceipt $ nor { norBlockNumber = 75 }
      go \case
        WaitSourceHeightFree lastHeight f -> do
          lastHeight `shouldBe` 75
          f 80
        RunNotarise seq h f -> do
          h `shouldBe` 80
          f ()
        o -> runStep (Just (noe 75, 98)) (Just back) bnd o
  
  describe "proposer sequence" do

    it "first" do
      go \case
        RunNotarise seq _ f -> do
          seq `shouldBe` 0
          f ()
        o -> runStep Nothing undefined bnd o

    it "forward" do
      go \case
        RunNotarise seq _ f -> do
          seq `shouldBe` 120
          f ()
        o -> runStep (Just (noe 75, 120)) (Just bnd) undefined o

    it "back" do
      go \case
        RunNotariseReceipt seq bnd' f -> do
          seq `shouldBe` 141
          f ()
        o -> runStep (Just (noe 0, 120)) Nothing bnd o
