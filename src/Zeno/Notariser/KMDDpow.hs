
module Zeno.Notariser.KMDDpow where

import qualified Data.Map as Map
import Data.Serialize

import qualified Haskoin as H

import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling

import Zeno.Console
import Zeno.Consensus
import Zeno.Notariser.Common.KMD
import Zeno.Notariser.Common
import Zeno.Notariser.Stats
import Zeno.Notariser.Types
import Zeno.Notariser.UTXO
import Zeno.Prelude


notariseKmdDpow :: NotariserConfig -> Int -> KomodoNotarisationReceipt -> Zeno EthNotariser ()
notariseKmdDpow nc@NotariserConfig{..} seq ndata = do
  withKomodoUtxo \utxo -> do
    KomodoIdent{..} <- asks has
    cparams <- getConsensusParamsWithStats nc EthToKmd
    let label = "eth ⇒  kmd"

    r <- ask :: Zeno EthNotariser EthNotariser
    let run = withContext (const r)

    txhash <- runConsensus label cparams outputs $ do
    
      proposal <- step "inputs" (collectWith $ proposeInputs (kmdNotarySigs sourceChain))
                                (kmdPubKeyI, getOutPoint utxo)

      Ballot _ _ utxos <- propose "inputs" (Just seq) $ pure proposal
    
      -- Sign tx and collect signed inputs
      let partlySignedTx = signMyInput nc kmdSecKey utxos outputs
          myInput = getMyInput utxo partlySignedTx
          waitCompileTx = collectWith $ collectTx partlySignedTx utxos
      finalTx <- step "sigs" waitCompileTx myInput

      _ <- step "confirm" collectMajority ()
    
      incStep "wait for tx confirm ..."
      run $ submitNotarisation nc finalTx

    dpowCheck nc txhash ndata

  where
  outputs = kmdDataOutputs outputAmount dpowRecip $ encode ndata
  outputAmount = kmdInputAmount -- Miners get the majority
  dpowRecip = H.PayPK "020e46e79a2a8d12b9b5d12c7a91adb4e454edfae43c0a0cb805427d2ac7613fd9"

type UTXO = (H.PubKeyI, H.OutPoint)

proposeInputs :: Int -> Int -> Inventory UTXO -> Maybe (Map Address UTXO)
                                                      -- I think maybe the addresses are redundant
                                                      -- in this map
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
       logInfo $ "Waiting for KMD height: " ++ show nextHeight
       withUIProc (UIOther $ "Waiting for KMD block %i" % nextHeight) do
         fix \f -> do
           threadDelay $ 5 * 1000000
           curHeight <- bitcoinGetHeight
           if curHeight < nextHeight
              then f
              else pure nextHeight

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
dpowCheck :: NotariserConfig -> H.TxHash -> KomodoNotarisationReceipt -> Zeno EthNotariser ()
dpowCheck NotariserConfig{..} txHash (KomodoNotarisationReceipt ndata) = do
  threadDelayS 1 -- We hope this isnt' neccesary
  height <- bitcoinGetTxHeight txHash >>=
    maybe (throwIO $ Inconsistent "Notarisation tx confirmed but could not get height") pure
  scanNotarisationsDB height (kmdSymbol sourceChain) 1 >>=
    \case
      Nothing -> throwIO $ Inconsistent "Notarisation tx not in notarisations db"
      Just (_, _, opret) | opret /= KomodoNotarisationReceipt ndata -> do
        logError $ show (opret, KomodoNotarisationReceipt ndata)
        throwIO $ Inconsistent "Notarisation in db has different opret"
      _ -> do
        logInfo "Transaction Confirmed"
