
module Zeno.Notariser.KMD where

import Network.Bitcoin
import Network.Komodo
import qualified Network.Haskoin.Prelude as H

import Zeno.Notariser.Types
import Zeno.Prelude


notariseToKMD :: Has BitcoinConfig r => KomodoUtxo -> Word32 -> Zeno r ()
notariseToKMD utxo height = undefined
--   KomodoIdent wif pk kmdAddr <- asks has
--   ident <- asks has
--   let cparams = ConsensusParams members ident consensusTimeout
--   r <- ask
--   let run = liftIO . runZeno r
--   let opret = Ser.encode ndata
-- 
--   runConsensus cparams opret $ do
-- 
--     -- Step 1 - Key on opret, collect UTXOs
--     run $ logDebug "Step 1: Collect inputs"
--     utxoBallots <- step waitMajority (pk, getOutPoint utxo)
-- 
--     -- Step 2 - TODO: Key on proposer
--     run $ logDebug "Step 2: Get proposed inputs"
--     utxosChosen <- propose $ pure $ proposeInputs cconf utxoBallots
-- 
--     -- Step 3 - Sign tx and collect signed inputs
--     run $ logDebug "Step 3: Sign & collect"
--     let signedTx = signMyInput wif utxosChosen $ H.DataCarrier opret
--         myInput = getMyInput utxo signedTx
--         waitSigs = waitOutpoints $ snd <$> utxosChosen
--     allSignedInputs <- step waitSigs myInput
--     let finalTx = compileFinalTx signedTx allSignedInputs
-- 
--     -- Step 4 - Confirm step 3 (bad attempt to overcome two generals problem)
--     run $ logDebug "Step 4: Just for kicks"
--     _ <- step waitMajority ()
-- 
--     run $ submitNotarisation cconf ndata finalTx



getKmdProposeHeight :: Has BitcoinConfig r => Word32 -> Zeno r Word32
getKmdProposeHeight n = do
  height <- bitcoinGetHeight
  pure $ height - mod height n

getKomodoUtxo :: Word64 -> Zeno NotariserConfig (Maybe KomodoUtxo)
getKomodoUtxo kmdInputAmount = do
  kmdAddress <- asks $ kmdAddress . has
  listToMaybe . choose <$> komodoUtxos [kmdAddress]
  where
  choose = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))
                   . filter ((==kmdInputAmount) . utxoAmount)
