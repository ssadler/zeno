
module Zeno.Notariser.KMD where

import Data.Serialize as Ser
import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling
import qualified Haskoin as H

import Zeno.Notariser.Types
import Zeno.Prelude
import Zeno.Consensus


kmdInputAmount :: Word64
kmdInputAmount = 9800


notariseToKMD :: NotariserConfig -> NotarisationData -> Zeno EthNotariser ()
notariseToKMD nc@NotariserConfig{..} ndata = do
  KomodoIdent{..} <- asks has

  utxo <- waitForUtxo

  cparams <- getConsensusParams nc
  r <- ask :: Zeno EthNotariser EthNotariser
  let run = withContext (const r)
  let opret = Ser.encode ndata

  runConsensus cparams opret $ do
  
    -- Key on opret, collect UTXOs
    utxoBallots <- step "inputs" (collectThreshold kmdNotarySigs) (kmdPubKeyI, getOutPoint utxo)

    -- TODO: Key on proposer
    let proposal = proposeInputs kmdNotarySigs $ unInventory utxoBallots
    Ballot _ _ utxosChosen <- propose "inputs" $ pure proposal
  
    -- Sign tx and collect signed inputs
    let partlySignedTx = signMyInput nc kmdSecKey utxosChosen $ H.DataCarrier opret
        myInput = getMyInput utxo partlySignedTx
        waitSigs = collectOutpoints $ snd <$> utxosChosen
    allSignedInputs <- step "sigs" waitSigs myInput
    let finalTx = compileFinalTx partlySignedTx $ unInventory allSignedInputs
  
    _ <- step "confirm" collectMajority ()
  
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

waitForUtxo :: Zeno EthNotariser KomodoUtxo
waitForUtxo = do
  getKomodoUtxo >>=
    \case Nothing -> logWarn "Waiting for UTXOs" >> threadDelay (10 * 1000000) >> waitForUtxo
          Just u -> pure u

notarisationRecip :: H.ScriptOutput
notarisationRecip = H.PayPK "020e46e79a2a8d12b9b5d12c7a91adb4e454edfae43c0a0cb805427d2ac7613fd9"
                    -- getAddrHash "RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA"

-- | Given selected UTXOs, compile a tx and sign own inputs, if any.
signMyInput :: NotariserConfig -> H.SecKey -> [(H.PubKeyI, H.OutPoint)] -> H.ScriptOutput -> SaplingTx
signMyInput NotariserConfig{..} wif ins opret = do
  let toSigIn (a, o) = H.SigInput (H.PayPK a) kmdInputAmount o H.sigHashAll Nothing
      inputs = toSigIn <$> ins
      outputAmount = kmdInputAmount * (fromIntegral $ length ins - 1)
      outputs = [(notarisationRecip, outputAmount), (opret, 0)]
      etx = saplingFromLegacy <$> H.buildTx (snd <$> ins) outputs
      signTx tx = signTxSapling komodo tx inputs [wif]
   in either error id $ etx >>= signTx

getMyInput :: KomodoUtxo -> SaplingTx -> Maybe H.TxIn
getMyInput myUtxo SaplingTx{..} =
  find (\txIn -> H.prevOutput txIn == getOutPoint myUtxo) txIn

compileFinalTx :: SaplingTx -> [Ballot (Maybe H.TxIn)] -> SaplingTx
compileFinalTx tx@SaplingTx{..} ballots = tx { txIn = mergedIns }
  where
    signedIns = catMaybes $ bData <$> ballots
    mischief = impureThrow $ ConsensusMischief $ "compileFinalTx: %s" % show (tx, ballots)
    mergedIns = map combine txIn
    combine unsigned =
      case find (\a -> H.prevOutput a == H.prevOutput unsigned) signedIns of
           Nothing -> mischief
           Just signed -> unsigned { H.scriptInput = H.scriptInput signed }

collectOutpoints :: [H.OutPoint] -> Collect (Maybe H.TxIn)
collectOutpoints given inv = do
  let getOPs = map H.prevOutput . catMaybes . map bData
      vals = getOPs $ unInventory inv
   in pure $ sortOn show vals == sortOn show given

submitNotarisation :: NotariserConfig -> NotarisationData -> SaplingTx -> Zeno EthNotariser ()
submitNotarisation NotariserConfig{..} ndata tx = do
  logInfo $ "Broadcast transaction: " ++ show (txHashSapling tx)
  -- TODO: Return the whole data structure from getrawtransaction
  txHash <- bitcoinSubmitTxSync 1 tx
  liftIO $ threadDelay $ 1 * 1000000

  -- Consistency check
  height <- bitcoinGetTxHeight txHash >>=
    maybe (throwIO $ Inconsistent "Notarisation tx confirmed but could not get height") pure
  scanNotarisationsDB height kmdChainSymbol 1 >>=
    \case
      Nothing -> throwIO $ Inconsistent "Notarisation tx not in notarisations db"
      Just n | opret n /= BND ndata -> do
        logError $ show (opret n, BND ndata)
        throwIO $ Inconsistent "Notarisation in db has different opret"
      _ -> do
        logInfo "Transaction Confirmed"
