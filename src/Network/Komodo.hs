{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Komodo where

import           Crypto.Secp256k1Wrapped
import           Data.Serialize as S

import           Network.Bitcoin
import qualified Haskoin as H
import           Haskoin.Constants

import           Zeno.Prelude
import           Zeno.Data.Aeson
import           Zeno.Data.Hex

import           UnliftIO

-- Komodo network constants --------------------------------------------------

komodo :: Network
komodo = btc
    { getNetworkName = "komodo"
    , getNetworkIdent = "kmd"
    , getAddrPrefix = 60
    , getScriptPrefix = 85
    , getSecretPrefix = 188
    , getExtPubKeyPrefix = 0x0488b21e
    , getExtSecretPrefix = 0x0488ade4
    , getCashAddrPrefix = Nothing
    , getBech32Prefix = Nothing
    }


-- Classic Address ------------------------------------------------------------

newtype RAddress = RAddress { getAddrHash :: H.Hash160 }

instance FromJSON RAddress where
  parseJSON val = do
    r <- H.addrFromJSON komodo val
    case r of
      H.PubKeyAddress hash -> pure $ RAddress hash
      _ -> fail ("Not a regalar address: " ++ show r)

instance ToJSON RAddress where
  toJSON = H.addrToJSON komodo . H.PubKeyAddress . getAddrHash

instance Show RAddress where
  show = toS . maybe (error "Shouldn't happen: couldn't encode RAddress") id . H.addrToString komodo . H.PubKeyAddress . getAddrHash

instance IsString RAddress where
  fromString s = maybe (error e) id $ stringToRAddress s
    where e = "Invalid Komodo address: " ++ show s

stringToRAddress :: String -> Maybe RAddress
stringToRAddress s =
  case H.stringToAddr komodo (fromString s) of
    Just (H.PubKeyAddress h160) -> Just (RAddress h160)
    _ -> Nothing

deriveKomodoAddress :: MonadUnliftIO m => PubKey -> m RAddress
deriveKomodoAddress pk = RAddress . H.addressHash <$> exportPubKey True pk



-- Ident ----------------------------------------------------------------------

data KomodoIdent = KomodoIdent
  { kmdSecKey :: SecKey
  , kmdPubKey :: PubKey
  , kmdPubKeyI :: H.PubKeyI
  , kmdAddress :: RAddress
  } deriving (Show)

deriveKomodoIdent :: MonadUnliftIO m => SecKey -> m KomodoIdent
deriveKomodoIdent kmdSecKey = do
  kmdPubKey <- derivePubKey kmdSecKey
  kmdAddress <- deriveKomodoAddress kmdPubKey
  let kmdPubKeyI = H.wrapPubKey True kmdPubKey
  pure $ KomodoIdent{..}


-- UTXOs ----------------------------------------------------------------------

listUnspentLogThresholdMs :: Int
listUnspentLogThresholdMs = 200

komodoListUnspent :: Has BitcoinConfig r => [RAddress] -> Zeno r [KomodoUtxo]
komodoListUnspent addrs = do
  whenSlow listUnspentLogThresholdMs
    do queryBitcoin "listunspent" (lo, hi, addrs)
    \ms -> logWarn $ "Komodo RPC call \"listunspent %i %i\" took %i ms" % (lo, hi, ms)
  where
  lo, hi :: Int
  lo = 1
  hi = 99999999


data KomodoUtxo = Utxo
  { utxoAmount :: Word64
  , utxoConfirmations :: Int
  , utxoTxid :: H.TxHash
  , utxoVout :: Word32
  , utxoAddress :: RAddress
  , utxoSpendable :: Bool
  } deriving (Show)

instance Eq KomodoUtxo where
  a == b = getOutPoint a == getOutPoint b

instance Ord KomodoUtxo where
  compare a b = compare (getOutPoint a) (getOutPoint b)

instance FromJSON KomodoUtxo where
  parseJSON val = do
    obj <- parseJSON val
    Number amount <- obj .: "amount"
    Utxo (floor $ amount * 1e8)
         <$> obj .: "confirmations"
         <*> obj .: "txid"
         <*> obj .: "vout"
         <*> obj .: "address"
         <*> obj .: "spendable"

getOutPoint :: KomodoUtxo -> H.OutPoint
getOutPoint utxo = H.OutPoint (utxoTxid utxo) (utxoVout utxo)





-- Notarisation Data ----------------------------------------------------------
--

data KomodoNotarisation = NOR
  { norBlockHash :: Bytes32
  , norBlockNumber :: Word32
  , norForeignRef :: Bytes32         -- Only present in encoded form in receipt
  , norSymbol :: String
  , norMom :: Bytes32
  , norMomDepth  :: Word16
  , norCcId :: Word16
  , norMomom :: Bytes32
  , norMomomDepth :: Word32
  } deriving (Eq, Show)

instance Serialize KomodoNotarisation where
  put nor@NOR{..} = encodeNotarisation isBack nor
    where isBack = norForeignRef /= minBound
  get = parseNotarisation False

encodeNotarisation :: Bool -> KomodoNotarisation -> Put
encodeNotarisation isBack NOR{..} = do
  put $ bytesReverse norBlockHash
  putWord32le norBlockNumber
  when isBack $ put $ bytesReverse norForeignRef
  mapM put norSymbol >> put '\0'
  put $ bytesReverse norMom
  putWord16le norMomDepth
  putWord16le norCcId
  when isBack do
    put $ bytesReverse norMomom
    putWord32le norMomomDepth

parseNotarisation :: Bool -> Get KomodoNotarisation
parseNotarisation isBack = do
  let getSymbol = get >>= \case '\0' -> pure ""; s -> (s:) <$> getSymbol
      getRev = bytesReverse <$> get

  norBlockHash <- getRev
  norBlockNumber <- getWord32le
  norForeignRef <- if isBack then getRev else pure nullBytes
  norSymbol <- getSymbol
  norMom <- getRev
  norMomDepth <- getWord16le
  norCcId <- getWord16le
  norMomom <- if isBack then getRev else pure nullBytes
  norMomomDepth <- if isBack then getWord32le else pure 0
  pure NOR{..}

instance FromJSON KomodoNotarisation where
  parseJSON = parseJsonHexSerialized

newtype KomodoNotarisationReceipt = KomodoNotarisationReceipt { unKNR :: KomodoNotarisation }
  deriving (Eq, Show)

instance Serialize KomodoNotarisationReceipt where
  put (KomodoNotarisationReceipt n) = encodeNotarisation True n
  get = KomodoNotarisationReceipt <$> parseNotarisation True

instance FromJSON KomodoNotarisationReceipt where
  parseJSON = parseJsonHexSerialized

-- Notarisation RPC

type KmdNotarisation n = (Word32, H.TxHash, n)

scanNotarisationsDB :: (FromJSON n, Has BitcoinConfig r)
                    => Word32 -> String -> Word32 -> Zeno r (Maybe (KmdNotarisation n))
scanNotarisationsDB height symbol limit = do
  traceE "scanNotarisationsDB" $ do
    val <- queryBitcoin "scanNotarisationsDB" [show height, symbol, show limit]
    pure $ if val == Null
              then Nothing
              else Just $ val .! "."

kmdGetLastNotarisation :: (FromJSON n, Has BitcoinConfig r) => String -> Zeno r (Maybe (KmdNotarisation n))
kmdGetLastNotarisation s = scanNotarisationsDB 0 s 100000

kmdGetLastNotarisationData :: (FromJSON n, Has BitcoinConfig r) => String -> Zeno r (Maybe n)
kmdGetLastNotarisationData s = (fmap (view _3)) <$> kmdGetLastNotarisation s

-- | Utils


parseJsonHexSerialized :: Serialize a => Value -> Parser a
parseJsonHexSerialized val = do
  Hex r <- parseJSON val
  either fail pure $ decode r

