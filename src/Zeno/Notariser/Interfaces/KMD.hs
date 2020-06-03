
module Zeno.Notariser.Interfaces.KMD where

import qualified Data.Map as Map
import Data.Serialize
import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling
import qualified Haskoin as H

import Network.Ethereum (Address, deriveEthAddress)
import Zeno.Consensus
import Zeno.Console
import Zeno.Notariser.Types
import Zeno.Prelude
import Zeno.Notariser.UTXO


kmdInputAmount :: Word64
kmdInputAmount = 9800


runKmdThreads :: Zeno EthNotariser ()
runKmdThreads = do
  forkMonitorUTXOs kmdInputAmount 5 20


kmdDataOutputs :: NotariserConfig -> H.ScriptOutput -> ByteString -> [H.TxOut]
kmdDataOutputs NotariserConfig{kmdNotarySigs} recip opret = 
  let outputAmount = kmdInputAmount * (fromIntegral $ kmdNotarySigs - 1)
   in [ H.TxOut outputAmount $ H.encodeOutputBS recip
      , H.TxOut 0 $ H.encodeOutputBS $ H.DataCarrier opret
      ]


notariseToKMD :: NotariserConfig -> String -> [H.TxOut] -> Zeno EthNotariser H.TxHash
notariseToKMD nc@NotariserConfig{..} label outputs = do
  KomodoIdent{..} <- asks has

  utxo <- waitForUtxo

  cparams <- getConsensusParams nc
  r <- ask :: Zeno EthNotariser EthNotariser
  let run = withContext (const r)

  runConsensus label cparams outputs $ do
  
    proposal <- step "inputs" (collectWith $ proposeInputs kmdNotarySigs)
                              (kmdPubKeyI, getOutPoint utxo)

    Ballot _ _ utxos <- propose "inputs" $ pure proposal
  
    -- Sign tx and collect signed inputs
    let partlySignedTx = signMyInput nc kmdSecKey utxos outputs
        myInput = getMyInput utxo partlySignedTx
        waitCompileTx = collectWith $ collectTx partlySignedTx utxos
    finalTx <- step "sigs" waitCompileTx myInput

    _ <- step "confirm" collectMajority ()
  
    incStep "wait for tx confirm ..."
    run $ submitNotarisation nc finalTx

type UTXO = (H.PubKeyI, H.OutPoint)

proposeInputs :: Int -> Int -> Inventory UTXO -> Maybe (Map Address UTXO)
proposeInputs kmdNotaryInputs _threshold ballots =
  if length ballots < kmdNotaryInputs
     then Nothing
     else Just $ snd <$> ballots

getNextHeight :: Word32 -> Word32 -> Word32 -> Word32
getNextHeight interval last current =
  let next = current - mod current interval
   in next + if next > last then 0 else interval

waitKmdNotariseHeight :: Has BitcoinConfig r => Word32 -> Word32 -> Zeno r Word32
waitKmdNotariseHeight interval lastHeight = do
  height <- bitcoinGetHeight
  let nextHeight = getNextHeight interval lastHeight height
  if nextHeight <= height
     then pure nextHeight
     else do
       withUIProc (UIOther $ "Waiting for KMD block %i" % nextHeight) do
         fix \f -> do
           threadDelay $ 5 * 1000000
           curHeight <- bitcoinGetHeight
           if curHeight < nextHeight
              then f
              else pure nextHeight


getKomodoUtxo :: Zeno EthNotariser (Maybe KomodoUtxo)
getKomodoUtxo = do
  kmdAddress <- asks $ kmdAddress . has
  listToMaybe . choose <$> komodoUtxos [kmdAddress]
  where
  choose = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))
                   . filter ((== kmdInputAmount) . utxoAmount)

waitForUtxo :: Zeno EthNotariser KomodoUtxo
waitForUtxo = do
  getKomodoUtxo >>= \case
    Just u -> pure u
    Nothing -> do
      logWarn "Waiting for UTXOs"
      withUIProc (UIOther "Waiting for UTXOs") do
        fix \f -> do
          threadDelay $ 10 * 1000000
          getKomodoUtxo >>= maybe f pure

-- | Given selected UTXOs, compile a tx and sign own inputs, if any.
signMyInput :: NotariserConfig -> H.SecKey -> Map Address UTXO -> [H.TxOut] -> SaplingTx
signMyInput NotariserConfig{..} wif mapIns outputs = do
  let toSigIn (a, o) = H.SigInput (H.PayPK a) kmdInputAmount o H.sigHashAll Nothing
      ins = Map.elems mapIns
      inputs = toSigIn <$> ins
      tx = saplingTx (snd <$> ins) outputs
      signTx tx = signTxSapling komodo tx inputs [wif]
   in either murphy id $ signTx tx

getMyInput :: KomodoUtxo -> SaplingTx -> Maybe H.TxIn
getMyInput myUtxo SaplingTx{..} =
  find (\txIn -> H.prevOutput txIn == getOutPoint myUtxo) txIn


collectTx :: SaplingTx -> Map Address UTXO
          -> Int -> Inventory (Maybe H.TxIn) -> Maybe SaplingTx
collectTx tx@SaplingTx{..} utxos threshold inventory = mtx
  where
    mtx = (\ins -> tx { txIn = ins }) <$> mapM f txIn
    f txin = do
      -- Why are chosenUTXOs and transaction separated?
      -- because the transaction txin helpfully has a bytestring script.
      let addr = maybe (murphy "lookup prevout") id $ Map.lookup (H.prevOutput txin) opIdx
      (sig, msigned) <- Map.lookup addr inventory
      msigned

    opIdx = Map.fromList [(op, addr) | (addr, (_, op)) <- Map.toList utxos]


submitNotarisation :: NotariserConfig -> SaplingTx -> Zeno EthNotariser H.TxHash
submitNotarisation NotariserConfig{..} tx = do
  logInfo $ "Broadcast transaction: " ++ show (txHashSapling tx)
  -- TODO: Return the whole data structure from getrawtransaction
  bitcoinSubmitTxSync 1 tx

 
-- | Validate assumptions
dpowCheck :: NotariserConfig -> H.TxHash -> NotarisationData -> Zeno EthNotariser ()
dpowCheck NotariserConfig{..} txHash ndata = do
  threadDelayS 1 -- We hope this isnt' neccesary
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
