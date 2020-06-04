
module Zeno.Notariser.Stats where

import Data.Serialize

import qualified Haskoin as H

import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling

import Zeno.Data.Aeson
import Zeno.Notariser.Types
import Zeno.Notariser.Common.KMD
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process

import Data.Time.Clock
import Data.Time.Calendar


outputAmount :: Word64
outputAmount = 1000 -- High enough not to trigger dust threshold


forkRecordProposerTimeout :: NotariserConfig -> Bool -> ProposerTimeout -> Consensus ()
forkRecordProposerTimeout _  False _ = logWarn "Timeout of fallback proposer"
forkRecordProposerTimeout nc True proposerTimeout = do
  roundId <- getRoundId
  cparams <- asks ccParams <&> \cp -> cp { onProposerTimeout' = Nothing }
  let label = "proposer timeout: " ++ show roundId
  t <- liftIO getCurrentTime
  let addr = statsAddress "proposer timeout" $ utctDay t
  let outputs = kmdDataOutputs outputAmount addr $ encode proposerTimeout
  
  spawnChildRound label cparams proposerTimeout do
    tx <- step "tx sigs" (collectWith $ collectTx outputs)
                         (sha256b $ encode outputs)
    (Ballot _ _ chosenTx) <- propose "tx sender" $ pure tx
    undefined

    -- txid <- bitcoinSubmitTxSync 0 chosenTx
    -- logInfo $ "Posted stats: \"%s\" (%s)" % (label, show txid)
    

collectTx :: [H.TxOut] -> Int -> Inventory Bytes32 -> Maybe SaplingTx
collectTx = undefined



statsAddress :: String -> Day -> H.ScriptOutput
statsAddress name day = 
  let s = name ++ show day
   in H.PayPKHash $ H.ripemd160 (toS s :: ByteString)
