
module Zeno.Notariser.Stats where

import Data.Serialize

import qualified Haskoin as H

import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling

import Zeno.Data.Aeson
import Zeno.Notariser.Interfaces.KMD
import Zeno.Notariser.Types
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process

import Data.Time.Clock
import Data.Time.Calendar


recordProposerTimeout :: NotariserConfig -> ProposerTimeout -> Zeno EthNotariser ()
recordProposerTimeout nc@NotariserConfig{..} pt = do
  let label = "proposer timeout: " ++ show (roundId pt)
  t <- liftIO getCurrentTime
  let addr = statsAddress "proposer timeout" $ utctDay t
  let outputs = kmdDataOutputs nc addr (encode pt)
  cparams <- getConsensusParams nc <&> \nc -> nc { onProposerTimeout' = Nothing }
  spawnNoHandle label do
    localZeno filterLogWarn do
      (Ballot _ _ chosenTx) <- 
        runConsensus label cparams pt do
          tx <- step "tx sigs" (collectWith $ collectTx outputs)
                               (sha256b $ encode outputs)
          propose "tx sender" $ pure tx

      txid <- bitcoinSubmitTxSync 0 chosenTx
      logInfo $ "Posted stats: \"%s\" (%s)" % (label, show txid)
  where
    filterLogWarn app =
      let Console _ status doEvents = appConsole app
       in app { appConsole = Console LevelWarn status False }


collectTx :: [H.TxOut] -> Int -> Inventory Bytes32 -> Maybe SaplingTx
collectTx = undefined



statsAddress :: String -> Day -> H.ScriptOutput
statsAddress name day = 
  let s = name ++ show day
      hash = H.ripemd160 (toS s :: ByteString)
   in H.PayPKHash hash
