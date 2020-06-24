
module Zeno.Notariser.KMDDpow where

import qualified Data.Map.Strict as Map
import Data.Serialize
import qualified Data.Set as Set

import qualified Haskoin as H

import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling
import Network.Ethereum (EthIdent(..))

import Zeno.Console
import Zeno.Consensus
import Zeno.Notariser.Collect
import Zeno.Notariser.Common.KMD
import Zeno.Notariser.Common
import Zeno.Notariser.Shuffle
import Zeno.Notariser.Types
import Zeno.Notariser.UTXO
import Zeno.Process
import Zeno.Prelude


notariseKmdDpow :: NotariserConfig -> String -> KomodoNotaryReceiptFromEth -> Zeno EthNotariser ()
notariseKmdDpow nc@NotariserConfig{..} label ndata = do
  withKomodoUtxo \utxo -> do
    KomodoIdent{..} <- asks has
    EthIdent{..} <- asks has
    cparams <- getConsensusParams nc EthToKmd

    r <- ask :: Zeno EthNotariser EthNotariser
    let run = withContext (const r)
        outputs = makeOutputs ndata

    runConsensus label cparams outputs do
      
      -- First complete an empty step to make sure everyone is on the same page
      _ <- step "init" () collectMajority

      dist@(proposer:_) <- roundShuffle members

      proposal <- step "inputs" (kmdPubKeyI, getOutPoint utxo) $
          if ethAddress == proposer
             then collectWeighted dist $ kmdNotarySigs sourceChain
             else collectWith \_ _ -> Just mempty


      Ballot _ _ utxos <- step "proposal" (snd <$> proposal) (collectMember proposer)
    
      let
        partlySignedTx = signMyInput nc kmdSecKeyH utxos outputs
        myInput = getMyInput utxo partlySignedTx
        waitCompileTx = collectWith $ collectTx partlySignedTx utxos

      -- Sign tx and collect signed inputs
      finalTx <- step "sigs" myInput waitCompileTx

      _ <- step "confirm" () collectMajority
    
      incStep "wait for tx confirm ..."
      run do
        txhash <- submitNotarisation nc finalTx
        dpowCheck nc txhash ndata

  where
  makeOutputs = kmdDataOutputs outputAmount dpowRecip . encode
  outputAmount = kmdInputAmount -- Miners get the majority
  dpowRecip = H.PayPK "020e46e79a2a8d12b9b5d12c7a91adb4e454edfae43c0a0cb805427d2ac7613fd9"

type UTXO = (H.PubKeyI, H.OutPoint)

collectInputs :: Int -> Int -> Inventory UTXO
              -> Maybe (Map Address UTXO)  -- I think maybe the addresses are redundant
                                           -- in this map
collectInputs kmdNotaryInputs _threshold ballots =
  if length ballots < kmdNotaryInputs
     then Nothing
     else Just $ snd <$> ballots



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
dpowCheck :: NotariserConfig -> H.TxHash -> KomodoNotaryReceiptFromEth -> Zeno EthNotariser ()
dpowCheck NotariserConfig{..} txHash ndata = do
  threadDelayS 1 -- We hope this isnt' neccesary
  height <- bitcoinGetTxHeight txHash >>=
    maybe (throwIO $ Inconsistent "Notarisation tx confirmed but could not get height") pure
  scanNotarisationsDB height (kmdSymbol sourceChain) 1 >>=
    \case
      Nothing -> throwIO $ Inconsistent "Notarisation tx not in notarisations db"
      Just (_, _, opret) | opret /= ndata -> do
        logError $ show (opret, ndata)
        throwIO $ Inconsistent "Notarisation in db has different opret"
      _ -> do
        logInfo "Transaction Confirmed"
