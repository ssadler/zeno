{-# LANGUAGE ForeignFunctionInterface #-}

module Network.ZCash.Sapling where

import Data.Bits
import Data.Serialize
import Crypto.Secp256k1 (SecKey)

import qualified Network.Haskoin.Prelude as H

import Crypto.Blake2
import Zeno.Data.Hex
import Zeno.Prelude



data SaplingTx = SaplingTx { unSaplingTx :: H.Tx }


signTxSapling
  :: H.Network
  -> H.Tx                      -- ^ transaction to sign
  -> [H.SigInput]              -- ^ signing parameters
  -> [SecKey]                  -- ^ private keys to sign with
  -> Either String SaplingTx   -- ^ signed transaction
signTxSapling net tx params keys = undefined



sigHashOverwintered
  :: Int
  -> H.SigInput
  -> H.Tx
  -> Either String ByteString
sigHashOverwintered inputIdx H.SigInput{..} tx = do

  when (inputIdx >= length txIns) $ Left "inputIdx out of range"
  when (sigInputOP /= H.prevOutput myIn) $ Left "prevOut mismatch"

  Right sigHash

  where

  txIns = H.txIn tx
  myIn = txIns !! inputIdx

  header = version .|. shiftL 1 31       :: Word32
  version = 4                            :: Word32
  versionGroupId = 0x892F2085            :: Word32
  expiryHeight = 0                       :: Word32
  consensusBranchId = 0x76b809bb         :: Word32

  personalization = runPut do
    putByteString zcash_sig_hash_personalization
    putWord32le consensusBranchId

  sigData = runPut do
    putWord32le header
    putWord32le versionGroupId
    putByteString prevOutsHash
    putByteString sequencesHash
    putByteString outputsHash
    put (nullBytes :: Bytes 32)           -- shielded inputs not supported
    put (nullBytes :: Bytes 32)           -- shielded outputs not supported
    putWord32le (H.txLockTime tx)
    putWord32le expiryHeight
    putWord64le 0                         -- valueBalance not supported
    putWord32le 1                         -- Constant sighashtype
    put sigInputOP
    put (H.encodeOutput sigInputScript)
    putWord64le sigInputValue
    putWord32le (H.txInSequence myIn)
  sigHash = blake2bPersonalized personalization sigData

  prevOutsData = runPut $ forM_ (H.txIn tx) $ put . H.prevOutput
  prevOutsHash = blake2bPersonalized zcash_prevouts_hash_personalization prevOutsData

  sequencesData = runPut $ forM_ (H.txIn tx) $ put . H.txInSequence
  sequencesHash = blake2bPersonalized zcash_sequence_hash_personalization sequencesData

  outputsData = runPut $ forM_ (H.txOut tx) put
  outputsHash = blake2bPersonalized zcash_outputs_hash_personalization outputsData

  zcash_join_splits_hash_personalization      = "ZcashJSplitsHash" :: ByteString
  zcash_outputs_hash_personalization          = "ZcashOutputsHash" :: ByteString
  zcash_prevouts_hash_personalization         = "ZcashPrevoutHash" :: ByteString
  zcash_sequence_hash_personalization         = "ZcashSequencHash" :: ByteString
  zcash_shielded_outputs_hash_personalization = "ZcashSOutputHash" :: ByteString
  zcash_shielded_spends_hash_personalization  = "ZcashSSpendsHash" :: ByteString
  zcash_sig_hash_personalization              = "ZcashSigHash"     :: ByteString

