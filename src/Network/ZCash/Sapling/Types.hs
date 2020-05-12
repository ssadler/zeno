
module Network.ZCash.Sapling.Types where



import Data.Bits
import Data.Serialize as S
import Data.Aeson

import qualified Haskoin as H
import qualified Haskoin.Transaction.Builder as H
import qualified Haskoin.Transaction.Builder.Sign as H

import Zeno.Prelude
import Zeno.Data.Hex

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

nullTx :: SaplingTx
nullTx = SaplingTx 4 [] [] 0

instance Serialize SaplingTx where
  get = do
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

  put SaplingTx{..} = do
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


instance FromJSON SaplingTx where
  parseJSON val = do
    Hex s <- parseJSON val
    either fail pure $ S.decode s

instance ToJSON SaplingTx where
  toJSON = String . toS . toHex . S.encode

txHashSapling :: SaplingTx -> H.TxHash
txHashSapling tx = H.TxHash (H.doubleSHA256 (S.encode tx))



newtype VarList a = VarList [a]

instance Serialize a => Serialize (VarList a) where
  get = do
    H.VarInt n <- get
    VarList <$> replicateM (fromIntegral n) get
  put (VarList l) = put (H.VarInt (fromIntegral $ length l)) >> mapM_ put l
