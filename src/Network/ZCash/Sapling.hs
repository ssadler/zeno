{-# LANGUAGE ForeignFunctionInterface #-}

module Network.ZCash.Sapling
  ( SaplingTx(..)
  , signTxSapling
  , txHashSapling
  ) where

import Data.Bits
import Data.Serialize as S
import Data.Aeson
import Crypto.Secp256k1 (SecKey)

import qualified Haskoin as H
import qualified Haskoin.Transaction.Builder as H
import qualified Haskoin.Transaction.Builder.Sign as H

import Crypto.Blake2
import Zeno.Data.Hex
import Zeno.Prelude

import Unsafe.Coerce


saplingHeader :: Word32
saplingHeader = 4 .|. shiftL 1 31

versionGroupId :: Word32
versionGroupId = 0x892F2085

expiryHeight :: Word32
expiryHeight = 0

valueBalance :: Word64
valueBalance = 0


-- TODO: Unwrap contents, doesn't support segwit
data SaplingTx = SaplingTx 
    { -- | transaction data format version
      txVersion  :: !Word32
      -- | list of transaction inputs
    , txIn       :: ![H.TxIn]
      -- | list of transaction outputs
    , txOut      :: ![H.TxOut]
      -- | earliest mining height or time
    , txLockTime :: !Word32
    } deriving (Show, Read, Eq)

saplingFromLegacy :: H.Tx -> SaplingTx
saplingFromLegacy H.Tx{..} = SaplingTx{..}

saplingToLegacy :: SaplingTx -> H.Tx
saplingToLegacy SaplingTx{..} = H.Tx{..} where txWitness = []

instance Serialize SaplingTx where
  get = parseSaplingTx
  put = putSapingTx

parseSaplingTx :: Get SaplingTx
parseSaplingTx = do
  h <- getWord32le
  when (h /= saplingHeader) $ fail $ "Not a v4 sapling tx: " ++ show h
  vg <- getWord32le
  when (vg /= versionGroupId) $ fail $ "Unexpected version group id: " ++ show vg
  VarList txIn <- get
  VarList txOut <- get
  txLockTime <- getWord32le
  expiryHeight <- getWord32le

  vb <- getWord64le
  H.VarInt ss <- get
  H.VarInt so <- get
  H.VarInt js <- get

  when (vb + ss + so + js /= 0) $ fail "Cannot parse ZCash inputs/outputs"

  let txVersion = 4
  pure $ SaplingTx{..}

  
putSapingTx :: SaplingTx -> Put
putSapingTx SaplingTx{..} = do
  putWord32le saplingHeader
  putWord32le versionGroupId
  put $ VarList txIn
  put $ VarList txOut
  putWord32le txLockTime
  putWord32le expiryHeight

  putWord64le 0  -- value balance
  H.putVarInt 0  -- shielded spends
  H.putVarInt 0  -- shielded outputs
  H.putVarInt 0  -- joinSplitSig



instance ToJSON SaplingTx where
  toJSON = String . toS . toHex . S.encode

txHashSapling :: SaplingTx -> H.TxHash
txHashSapling tx = H.TxHash (H.doubleSHA256 (S.encode tx))


signTxSapling
  :: H.Network
  -> H.Tx                      -- ^ transaction to sign
  -> [H.SigInput]              -- ^ signing parameters
  -> [SecKey]                  -- ^ private keys to sign with
  -> Either String SaplingTx   -- ^ signed transaction
signTxSapling net otx sigis allKeys =
  saplingFromLegacy <$> signTx net otx sigis allKeys


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

  sigData = runPut do
    putWord32le saplingHeader
    putWord32le versionGroupId
    putByteString prevOutsHash
    putByteString sequencesHash
    putByteString outputsHash
    put (nullBytes :: Bytes 32)           -- shielded inputs not supported
    put (nullBytes :: Bytes 32)           -- shielded outputs not supported
    putWord32le (H.txLockTime tx)
    putWord32le expiryHeight
    putWord64le 0                         -- Don't support shielded spends
    putWord32le 1                         -- Constant sighashtype
    put sigInputOP
    put (H.encodeOutput sigInputScript)
    putWord64le sigInputValue
    putWord32le (H.txInSequence myIn)
  sigHash = blake2bPersonalized personalization sigData

  -- TODO: Do these list puts need to be length prefixed?
  
  prevOutsData = runPut $ forM_ (H.txIn tx) $ put . H.prevOutput
  prevOutsHash = blake2bPersonalized zcash_prevouts_hash prevOutsData

  sequencesData = runPut $ forM_ (H.txIn tx) $ put . H.txInSequence
  sequencesHash = blake2bPersonalized zcash_sequence_hash sequencesData

  outputsData = runPut $ forM_ (H.txOut tx) put
  outputsHash = blake2bPersonalized zcash_outputs_hash outputsData

  zcash_join_splits_hash      = "ZcashJSplitsHash" :: ByteString
  zcash_outputs_hash          = "ZcashOutputsHash" :: ByteString
  zcash_prevouts_hash         = "ZcashPrevoutHash" :: ByteString
  zcash_sequence_hash         = "ZcashSequencHash" :: ByteString
  zcash_shielded_outputs_hash = "ZcashSOutputHash" :: ByteString
  zcash_shielded_spends_hash  = "ZcashSSpendsHash" :: ByteString
  zcash_sig_hash              = "ZcashSigHash"     :: ByteString


-- | Utility types

newtype VarList a = VarList [a]

instance Serialize a => Serialize (VarList a) where
  get = do
    H.VarInt n <- get
    VarList <$> replicateM (fromIntegral n) get
  put (VarList l) = put (H.VarInt (fromIntegral $ length l)) >> mapM_ put l



-- | Here and below are the functions we need to modify to support Sapling

signTx
  :: H.Network
  -> H.Tx                      -- ^ transaction to sign
  -> [H.SigInput]              -- ^ signing parameters
  -> [SecKey]                  -- ^ private keys to sign with
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


