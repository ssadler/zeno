
module Zeno.Notariser.KMD where

import Data.Serialize as Ser
import Network.Bitcoin
import Network.Komodo
import qualified Network.Haskoin.Prelude as H

import Zeno.Notariser.Types
import Zeno.Prelude
import Zeno.Prelude.Lifted
import Zeno.Consensus


kmdInputAmount :: Word64
kmdInputAmount = 9800


notariseToKMD :: NotariserConfig -> NotarisationData -> Zeno EthNotariser ()
notariseToKMD nc@NotariserConfig{..} ndata = do

  utxo <- getKomodoUtxo <&> maybe (error "No UTXOs!") id


  KomodoIdent wif pk _ <- asks has
  cparams <- getConsensusParams nc
  r <- ask :: Zeno EthNotariser EthNotariser
  let run = liftIO . runZeno r
  let opret = Ser.encode ndata

  runConsensus cparams opret $ do
  
    -- Step 1 - Key on opret, collect UTXOs
    run $ logDebug "Step 1: Collect inputs"
    utxoBallots <- step collectMajority (pk, getOutPoint utxo)
  
    -- Step 2 - TODO: Key on proposer
    run $ logDebug "Step 2: Get proposed inputs"
    let proposal = proposeInputs kmdNotarySigs $ unInventory utxoBallots
    Ballot _ _ utxosChosen <- propose $ pure proposal
  
    -- Step 3 - Sign tx and collect signed inputs
    run $ logDebug "Step 3: Sign & collect"
    let partlySignedTx = signMyInput nc wif utxosChosen $ H.DataCarrier opret
        myInput = getMyInput utxo partlySignedTx
        waitSigs = collectOutpoints $ snd <$> utxosChosen
    allSignedInputs <- step waitSigs myInput
    let finalTx = compileFinalTx partlySignedTx $ unInventory allSignedInputs
  
    -- Step 4 - Confirm step 3 (doesn't overcome two generals problem, we let other chains do that)
    run $ logDebug "Step 4: Confirm"
    _ <- step collectMajority ()
  
    run $ submitNotarisation nc ndata finalTx


proposeInputs :: Int -> [Ballot (H.PubKeyI, H.OutPoint)] -> [(H.PubKeyI, H.OutPoint)]
proposeInputs kmdNotaryInputs ballots
  | length ballots < kmdNotaryInputs = error "Bad error: not enough ballots"
  | otherwise = take kmdNotaryInputs $ bData <$> sortOn bSig ballots


getKmdProposeHeight :: Has BitcoinConfig r => Word32 -> Zeno r Word32
getKmdProposeHeight n = do
  height <- bitcoinGetHeight
  pure $ height - mod height n

getKomodoUtxo :: Zeno EthNotariser (Maybe KomodoUtxo)
getKomodoUtxo = do
  kmdAddress <- asks $ kmdAddress . has
  listToMaybe . choose <$> komodoUtxos [kmdAddress]
  where
  choose = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))
                   . filter ((== kmdInputAmount) . utxoAmount)

notarisationRecip :: H.ScriptOutput
notarisationRecip = H.PayPK "020e46e79a2a8d12b9b5d12c7a91adb4e454edfae43c0a0cb805427d2ac7613fd9" -- $ getAddrHash "RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA"

-- | Given selected UTXOs, compile a tx and sign own inputs, if any.
signMyInput :: NotariserConfig -> H.SecKey -> [(H.PubKeyI, H.OutPoint)] -> H.ScriptOutput -> H.Tx
signMyInput NotariserConfig{..} wif ins opret = do
  let toSigIn (a, o) = H.SigInput (H.PayPK a) kmdInputAmount o H.sigHashAll Nothing
      inputs = toSigIn <$> ins
      outputAmount = kmdInputAmount * (fromIntegral $ length ins - 1)
      outputs = [(notarisationRecip, outputAmount), (opret, 0)]
      etx = H.buildTx (snd <$> ins) outputs
      signTx tx = H.signTx komodo tx inputs [wif]
   in either error id $ etx >>= signTx

getMyInput :: KomodoUtxo -> H.Tx -> Maybe H.TxIn
getMyInput myUtxo tx =
  find (\txIn -> H.prevOutput txIn == getOutPoint myUtxo)
       (H.txIn tx)

compileFinalTx :: H.Tx -> [Ballot (Maybe H.TxIn)] -> H.Tx
compileFinalTx tx ballots = tx { H.txIn = mergedIns }
  where
    signedIns = catMaybes $ bData <$> ballots
    unsignedIns = H.txIn tx
    mischief = impureThrow $ ConsensusMischief $ "compileFinalTx: %s" % show (tx, ballots)
    mergedIns = map combine unsignedIns
    combine unsigned =
      case find (\a -> H.prevOutput a == H.prevOutput unsigned) signedIns of
           Nothing -> mischief
           Just signed -> unsigned { H.scriptInput = H.scriptInput signed }

collectOutpoints :: [H.OutPoint] -> Collect (Maybe H.TxIn)
collectOutpoints given = collectGeneric test
  where test _ inv =
          let getOPs = map H.prevOutput . catMaybes . map bData
              vals = getOPs $ unInventory inv
           in sortOn show vals == sortOn show given

submitNotarisation :: NotariserConfig -> NotarisationData -> H.Tx -> Zeno EthNotariser ()
submitNotarisation NotariserConfig{..} ndata tx = do
  logInfo $ "Broadcast transaction: " ++ show (H.txHash tx)
  bitcoinSubmitTxSync 1 tx
  liftIO $ threadDelay $ 1 * 1000000

  -- Consistency check
  mln <- getLastNotarisation kmdChainSymbol
  when ((opret <$> mln) /= Just (BND ndata)) $ do
     logError "Bad error. Notarisation tx confirmed but didn't show up in db."
     logError $ show (ndata, mln)
     error "Bailing"

  logInfo "Transaction Confirmed"
