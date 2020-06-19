
module Network.ZCash.Sapling.Sign where

import qualified Data.ByteString as BS
import Crypto.Blake2
import Data.Serialize as S
import qualified Haskoin as H
import qualified Haskoin.Transaction.Builder as H
import qualified Haskoin.Transaction.Builder.Sign as H

import Network.ZCash.Sapling.Types
import Zeno.Data.Hex
import Zeno.Prelude


signTxSapling
  :: H.Network
  -> SaplingTx                   -- ^ transaction to sign
  -> [H.SigInput]                -- ^ signing parameters
  -> [H.SecKey]                  -- ^ private keys to sign with
  -> Either String SaplingTx     -- ^ signed transaction
signTxSapling net otx sigis allKeys =
  saplingFromLegacy <$> signTx net (saplingToLegacy otx) sigis allKeys


makeSigHashOverwintered
  :: H.Network
  -> Int
  -> H.SigInput
  -> H.Tx
  -> Either String ByteString
makeSigHashOverwintered net inputIdx H.SigInput{..} tx
  | sigInputSH /= H.sigHashAll = Left "Only SIGHASH_ALL supported for Sapling"
  | inputIdx >= length txIns = Left "inputIdx out of range"
  | sigInputOP /= H.prevOutput myIn = Left "prevOut mismatch"
  | otherwise = Right sigHash
  where
  txIns = H.txIn tx
  myIn = txIns !! inputIdx

  version = 4
  consensusBranchId = 0x76b809bb

  personalization = runPut do
    putByteString zcash_sig_hash
    putWord32le consensusBranchId

  sigHash = blake2bPersonalized personalization sigData
  sigData = runPut do
    putWord32le saplingHeader
    putWord32le versionGroupId
    putByteString prevOutsHash
    putByteString sequencesHash
    putByteString outputsHash
    put (nullBytes :: Bytes32)           -- join splits not supported
    put (nullBytes :: Bytes32)           -- shielded inputs not supported
    put (nullBytes :: Bytes32)           -- shielded outputs not supported
    putWord32le (H.txLockTime tx)
    putWord32le expiryHeight
    putWord64le 0                         -- Don't support shielded spends
    putWord32le 1                         -- Constant sighashtype
    put sigInputOP
    putScriptOutput sigInputScript
    putWord64le sigInputValue
    putWord32le (H.txInSequence myIn)
  
  prevOutsHash = blake2bPersonalized zcash_prevouts_hash prevOutsData
  prevOutsData = runPut $ forM_ (H.txIn tx) $ put . H.prevOutput

  sequencesHash = blake2bPersonalized zcash_sequence_hash sequencesData
  sequencesData = runPut $ forM_ (H.txIn tx) $ put . H.txInSequence

  outputsHash = blake2bPersonalized zcash_outputs_hash outputsData
  outputsData = runPut $ forM_ (H.txOut tx) put

  zcash_join_splits_hash      = "ZcashJSplitsHash" :: ByteString
  zcash_outputs_hash          = "ZcashOutputsHash" :: ByteString
  zcash_prevouts_hash         = "ZcashPrevoutHash" :: ByteString
  zcash_sequence_hash         = "ZcashSequencHash" :: ByteString
  zcash_shielded_outputs_hash = "ZcashSOutputHash" :: ByteString
  zcash_shielded_spends_hash  = "ZcashSSpendsHash" :: ByteString
  zcash_sig_hash              = "ZcashSigHash"     :: ByteString

  putScriptOutput so = do
    putCompactSize $ BS.length bs
    putByteString bs
    where
    bs = H.encodeOutputBS so
    putCompactSize n
      | n < 253    = putWord8 (fromIntegral n)
      | n < (2^16) = putWord8 253 >> putWord16le (fromIntegral n)
      | n < (2^32) = putWord8 254 >> putWord32le (fromIntegral n)
      | otherwise  = putWord8 255 >> putWord64le (fromIntegral n)


-- | Here and below are functions borrowed from Haskoin that
--   we needed to modify to call saplingSigHash


signTx
  :: H.Network
  -> H.Tx                      -- ^ transaction to sign
  -> [H.SigInput]              -- ^ signing parameters
  -> [H.SecKey]                  -- ^ private keys to sign with
  -> Either String H.Tx        -- ^ signed transaction
signTx net otx sigis allKeys
    | null ti   = Left "signTx: Transaction has no inputs"
    | otherwise = foldM go otx $ H.findInputIndex H.sigInputOP sigis ti
  where
    ti = H.txIn otx
    go :: H.Tx -> (H.SigInput, Int) -> Either String H.Tx
    go tx (sigi@(H.SigInput so _ _ _ rdmM), i) = do
        keys <- H.sigKeys so rdmM allKeys
        foldM (\t k -> signInput net t i sigi k) tx keys

signInput ::
       H.Network
    -> H.Tx
    -> Int
    -> H.SigInput
    -> H.SecKeyI
    -> Either String H.Tx
signInput net tx i sigIn@(H.SigInput so val _ _ rdmM) key = do
    sig <- makeSignature net tx i sigIn key
    si <- H.buildInput net tx i so val rdmM sig $ H.derivePubKeyI key
    nextIn <- nextTxIn so si
    pure $ tx { H.txIn = nextIn }
  where
    f si x = x { H.scriptInput = H.encodeInputBS si }
    g so' x = x { H.scriptInput = S.encode . H.opPushData $ H.encodeOutputBS so' }
    txis = H.txIn tx
    nextTxIn so' si
      | H.isSegwit so' = Left "Segwit not supported in sapling txs"
      | otherwise      = pure $ H.updateIndex i txis (f si)

-- | Produce a structured representation of a deterministic (RFC-6979) signature over an input.
makeSignature :: H.Network -> H.Tx -> Int -> H.SigInput -> H.SecKeyI -> Either String H.TxSignature
makeSignature net tx i sigin@(H.SigInput _ _ _ sh _) key = do
    m <- makeSigHashOverwintered net i sigin tx
    m' <- S.decode m
    pure $ H.TxSignature (H.signHash (H.secKeyData key) m') sh


