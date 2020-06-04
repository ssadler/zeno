
module Zeno.Notariser.Stats where

import Data.Serialize
import Data.Time.Clock
import Data.Time.Calendar

import qualified Haskoin as H

import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling

import UnliftIO

import Zeno.Data.Aeson
import Zeno.Notariser.Common
import Zeno.Notariser.Common.KMD
import Zeno.Notariser.Types
import Zeno.Notariser.UTXO
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process


outputAmount :: Word64
outputAmount = 1000 -- High enough not to look like dust hopefully


getConsensusParamsWithStats :: NotariserConfig -> Zeno EthNotariser ConsensusParams
getConsensusParamsWithStats nc = do
  -- Importantly, this captures the resource context so that any thread spawned
  -- will be a child of the current thread, not the thread where the action
  -- is called.
  params <- getConsensusParams nc
  rio <- askRunInIO
  let opt = rio . forkRecordProposerTimeout nc
  pure $ params { onProposerTimeout' = Just opt }


forkRecordProposerTimeout :: NotariserConfig -> ProposerTimeout -> Zeno EthNotariser ()
forkRecordProposerTimeout nc proposerTimeout = do

  let label = "proposer timeout: " ++ show (roundId proposerTimeout)
  spawnNoHandle label do
    localZeno (console . writeStatusEvents .~ False) do

      utxo <- waitForUtxo
      KomodoIdent{..} <- asks has
      t <- liftIO getCurrentTime

      let
        markerAddr = statsAddress "proposer timeout" $ utctDay t
        payload = encode proposerTimeout
        outputsToSign = kmdDataOutputs outputAmount markerAddr payload
        collect = collectWith \t inv -> do
          guard $ length inv >= t
          let sigs = [s | (s, _) <- toList inv]
          let signedPayload = encode (sigs, proposerTimeout)
          let outputs' = kmdDataOutputs outputAmount markerAddr signedPayload
          let outpoint = getOutPoint utxo
          let tx = saplingTx [outpoint] outputs'
          let sigIn = H.SigInput (H.PayPK kmdPubKeyI) kmdInputAmount outpoint H.sigHashAll Nothing
          let signed = either murphy id $ signTxSapling komodo tx [sigIn] [kmdSecKey]
          Just signed
      
      cparams <- getConsensusParams nc
      (Ballot _ _ chosenTx) <- 
        runConsensus label cparams proposerTimeout do
          tx <- step "tx sigs" collect (sha256b $ encode outputsToSign)
          propose "tx sender" Nothing $ pure tx

      txid <- bitcoinSubmitTxSync 0 chosenTx
      logInfo $ "Posted stats: \"%s\" (%s)" % (label, show txid)


statsAddress :: String -> Day -> H.ScriptOutput
statsAddress name day = 
  let s = name ++ show day
   in H.PayPKHash $ H.ripemd160 (toS s :: ByteString)
