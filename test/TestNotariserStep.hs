
module TestNotariserStep where

import TestUtils

import Zeno.Notariser.Types
import Zeno.Notariser.KMDETH

import Control.Monad.Trans.Free
import Control.Monad.Logger

import Debug.Trace



test_notariser_step :: TestTree
test_notariser_step = testGroup "test notariser step"
  [
    testCase "1" do
      let e = error . show
      let nc = NotariserConfig (e 1) (e 2) (e 3) (e 4) (e 5) (e 6) (e 7) (e 8) (e 9)
      (Free (RunNotarise _ _ _)) <- runNoLoggingT $ runFreeT $ notariserStepFree nc
      pure ()
  ]


-- runTestNotariserStep :: NotariserStep (Zeno EthNotariser) a -> Zeno EthNotariser a
-- runTestNotariserStep nc@NotariserConfig{..} = iterT
--   \case
--     RunNotarise seq height f       -> notariseToETH nc seq height >>= f
--     RunNotariseReceipt seq opret f -> notariseKmdDpow nc seq opret >>= f
--     WaitSourceHeightFree height f  -> waitKmdNotariseHeight kmdBlockInterval height >>= f
--     GetLastNotarisationReceipt f   -> kmdGetLastNotarisation kmdChainSymbol >>= f
--     HandleTimeoutFree act f        -> handle (\ConsensusTimeout -> pure ()) (runNotariserStep nc act) >>= f
-- 
--     GetLastNotarisationFree f -> do
--       (r, sequence) <- ethCallABI notarisationsContract "getLastNotarisation()" ()
--       f $ case r of
--         NOE 0 _ _ _ -> Nothing
--         _           -> Just (r, ProposerSequence sequence)
-- 
--     MakeNotarisationReceipt NOE{..} f  -> do
--       f $ NOR
--         { blockHash = (sha3AsBytes32 foreignHash)
--         , blockNumber = foreignHeight
--         , txHash = newFixed 0xFF
--         , symbol = kmdChainSymbol
--         , mom = nullBytes
--         , momDepth = 0
--         , ccId = 0
--         , momom = nullBytes
--         , momomDepth = 0
--         }
