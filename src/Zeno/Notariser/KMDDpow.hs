
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
    let outputs = makeOutputs ndata

    join do
      runConsensus label cparams outputs do
        
        -- First complete an empty step to make sure everyone is on the same page
        _ <- step "init" () collectMajority

        dist@(proposer:_) <- roundShuffle members
        logDebug $ "Proposer is: " ++ show proposer

        proposal <- step "utxos" (kmdPubKeyI, getOutPoint utxo) $
            if ethAddress == proposer
               then collectWeighted dist $ kmdNotarySigs sourceChain
               else collectWith \_ _ -> pure (Just mempty)

        Ballot _ _ utxos <- step "proposal" (snd <$> proposal) (collectMember proposer)
      
        let
          partlySignedTx = signMyInput nc kmdSecKeyH utxos outputs
          myInput = getMyInput utxo partlySignedTx
          waitCompileTx = collectTx partlySignedTx utxos

        -- Sign tx and collect signed inputs
        finalTx <- stepOptData "sigs" myInput waitCompileTx

        _ <- step "confirm" () collectMajority
      
        pure do
          sendUI $ UI_Step "Confirm TX"
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

kmdDataOutputs :: Word64 -> H.ScriptOutput -> ByteString -> [H.TxOut]
kmdDataOutputs amount recip opret = 
   [ H.TxOut amount $ H.encodeOutputBS recip
   , H.TxOut 0 $ H.encodeOutputBS $ H.DataCarrier opret
   ]

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


collectTx :: SaplingTx -> Map Address UTXO -> Collect H.TxIn (Zeno r) SaplingTx
collectTx tx@SaplingTx{..} utxos recv = do
  let addrsNeeded = lookupAddr <$> txIn
  ballots <- collectMembers addrsNeeded recv
  pure $ tx { txIn = bData <$> ballots }
  where
    -- Unfortunate murphy here, because our utxo map should have all the utxos in the tx
    lookupAddr txin = maybe (murphy "lookup prevout") id $ Map.lookup (H.prevOutput txin) opIdx
    opIdx = Map.fromList [(op, addr) | (addr, (_, op)) <- Map.toList utxos]


submitNotarisation :: NotariserConfig -> SaplingTx -> Zeno EthNotariser H.TxHash
submitNotarisation NotariserConfig{..} tx = do
  logInfo $ "Sending transaction: " ++ show (txHashSapling tx)
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
