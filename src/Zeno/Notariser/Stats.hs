
module Zeno.Notariser.Stats where

import qualified Data.Aeson as AE
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List ((\\), nub)
import Data.Serialize
import Data.Time.Clock
import Data.Time.Calendar

import qualified Haskoin as H

import Network.Ethereum.Crypto (Address, CompactRecSig, sha3b, recoverAddr)
import Network.Ethereum (GethConfig)
import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling

import System.IO

import UnliftIO

import Zeno.Data.Aeson
import Zeno.EthGateway
import Zeno.Notariser.Common
import Zeno.Notariser.Common.KMD
import Zeno.Notariser.Types
import Zeno.Notariser.UTXO
import Zeno.Consensus
import Zeno.Prelude
import Zeno.Process


outputAmount :: Word64
outputAmount = 1000 -- High enough not to look like dust hopefully


getConsensusParamsWithStats :: NotariserConfig -> RoundType -> Zeno EthNotariser ConsensusParams
getConsensusParamsWithStats nc roundType = do
  -- Importantly, this captures the resource context so that any thread spawned
  -- will be a child of this thread, not the thread where the action is called.
  params <- getConsensusParams nc roundType
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
        markerAddr = H.PayPKHash $ proposerTimeoutAddress $ utctDay t
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

      cparams <- getConsensusParams nc StatsToKmd

      (Ballot _ _ chosenTx) <- 

        runConsensus label cparams proposerTimeout do
          tx <- step "tx sigs" collect (sha256b $ encode outputsToSign)
          propose "tx sender" Nothing $ pure tx

      txid <- bitcoinSubmitTxSync 0 chosenTx
      logInfo $ "Posted stats: \"%s\" (%s)" % (label, show txid)


proposerTimeoutAddress :: Day -> H.Hash160
proposerTimeoutAddress = statsAddress "proposer timeout"


statsAddress :: String -> Day -> H.Hash160
statsAddress name day = 
  let s = name ++ show day
   in H.ripemd160 (toS s :: ByteString)


runDumpProposerTimeouts :: FilePath -> Address -> GethConfig -> Int -> IO ()
runDumpProposerTimeouts kmdConfPath gateway gethConfig numDays = do
  today <- utctDay <$> getCurrentTime
  let startDay = toEnum $ fromEnum today - max 0 (numDays - 1)

  processed <-
    runZeno stderrLog () do
      bitcoinConf <- loadBitcoinConfig kmdConfPath
      group <- withContext (const gethConfig) $ gatewayGetMembers gateway
      withContext (const bitcoinConf) do
        forM [startDay..today] \day -> do
          let address = RAddress $ proposerTimeoutAddress day
          utxos <- queryBitcoin "getaddressutxos" [address]
          mrecords <- mapM (loadRecord group) $ sortOn height utxos
          records <- dedupe $ catMaybes mrecords
          forM_ records \(marker, r) -> do
            let output = "{date,timeout,marker}" .% (day, r, marker)
            liftIO $ BSL8.putStrLn $ AE.encode output
          pure $ length mrecords

  when (null processed) do
    hPutStrLn stderr "No records found, is the chain being run with -addressindex=1?"
  pure ()

  where
  loadRecord :: Has BitcoinConfig r
             => (Int, [Address])
             -> MarkerUtxo
             -> Zeno r (Maybe (MarkerUtxo, Value))
  loadRecord (threshold, members) marker@Marker{..} = do
    SaplingTx _ _ outs _ <- queryBitcoin "getrawtransaction" [txid]

    r <-
      runExceptT do
        when (length outs /= 2) $ throwError $ "Wrong number of outputs (%i)" % length outs
        let [marker, H.TxOut _ script] = outs
        script <- liftEither $ H.decodeOutputBS script
        opret <-
          case script of
            H.DataCarrier bs -> pure bs
            other -> throwError "Output 1 not a data carrier"

        (sigs, payload) <- liftEither $ decode opret
        let markerAddress = H.PayPKHash $ getAddrHash address
        let outputsToSign = kmdDataOutputs outputAmount markerAddress $ encode payload
        let message = sha256b $ encode outputsToSign
        let addrs = catMaybes $ recoverAddr message <$> sigs

        when (length sigs < threshold) do
          throwError "Not enough signers"
        when (length addrs /= length sigs) do
          throwError "Signature recovery failure"
        when (length (nub addrs) /= length sigs) do
          throwError "Duplicate sigs detected"
        when (addrs \\ members /= []) do
          throwError "Non member sigs present"

        case eitherDecode' payload of
          Right p -> pure p
          Left e -> murphy "Payload is signed but not valid JSON"

    case r of
      Right o -> pure $ Just (marker, o)
      Left err -> do
        logWarn $ "txid %s %s" % (show txid, err)
        pure Nothing

  dedupe recs = do
    let recs' = nub (EqOnSnd <$> recs)
        ndupes = length recs - length recs'
    when (ndupes /= 0) do
      logWarn $ "Duplicates encountered! %i" % ndupes
    pure $ unEqOnSnd <$> recs'

newtype EqOnSnd a b = EqOnSnd { unEqOnSnd :: (a, b) }
instance Eq b => Eq (EqOnSnd a b) where (EqOnSnd (_, b)) == (EqOnSnd (_, b')) = b == b'

data MarkerUtxo = Marker { txid :: H.TxHash , height :: Word32 , address :: RAddress }
  deriving (Generic)
instance FromJSON MarkerUtxo
instance ToJSON MarkerUtxo
