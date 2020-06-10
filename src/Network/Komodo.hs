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

data NotarisationData = NOR
  { blockHash :: Bytes32
  , blockNumber :: Word32
  , txHash :: Bytes32 -- for back notarisation
  , symbol :: String
  , mom :: Bytes32
  , momDepth  :: Word16
  , ccId :: Word16
  , momom :: Bytes32
  , momomDepth :: Word32
  } deriving (Eq, Show)

momDepth32 :: Integral a => NotarisationData -> a
momDepth32 = fromIntegral . momDepth

instance Serialize NotarisationData where
  put nor@NOR{..} = encodeNotarisation isBack nor
    where isBack = txHash /= nullBytes
  get = parseNotarisation False

encodeNotarisation :: Bool -> NotarisationData -> Put
encodeNotarisation isBack NOR{..} = do
  put $ bytesReverse blockHash
  putWord32le blockNumber
  when isBack $ put $ bytesReverse txHash
  mapM put symbol >> put '\0'
  put $ bytesReverse mom
  putWord16le momDepth
  putWord16le ccId
  when isBack do
    put $ bytesReverse momom
    putWord32le momomDepth

parseNotarisation :: Bool -> Get NotarisationData
parseNotarisation isBack = do
  let getSymbol = get >>= \case '\0' -> pure ""; s -> (s:) <$> getSymbol
      getRev = bytesReverse <$> get

  blockHash <- getRev
  blockNumber <- getWord32le
  txHash <- if isBack then getRev else pure nullBytes
  symbol <- getSymbol
  mom <- getRev
  momDepth <- getWord16le
  ccId <- getWord16le
  momom <- if isBack then getRev else pure nullBytes
  momomDepth <- if isBack then getWord32le else pure 0
  pure NOR{..}

instance FromJSON NotarisationData where
  parseJSON = parseJsonHexSerialized

newtype BackNotarisationData = BND NotarisationData
  deriving (Eq, Show)

instance Serialize BackNotarisationData where
  put (BND n) = encodeNotarisation True n
  get = BND <$> parseNotarisation True

instance FromJSON BackNotarisationData where
  parseJSON = parseJsonHexSerialized

-- Notarisation RPC

data Notarisation n = Notarisation
  { kmdHeight :: Word32
  , nTxHash :: H.TxHash
  , opret :: n
  } deriving (Show)

instance FromJSON n => FromJSON (Notarisation n) where
  parseJSON val = do
    obj <- parseJSON val
    Notarisation <$> obj .: "height"
                 <*> obj .: "hash"
                 <*> obj .: "opreturn"

scanNotarisationsDB :: (FromJSON n, Has BitcoinConfig r)
                    => Word32 -> String -> Word32 -> Zeno r (Maybe (Notarisation n))
scanNotarisationsDB height symbol limit = do
  traceE "scanNotarisationsDB" $ do
    val <- queryBitcoin "scanNotarisationsDB" [show height, symbol, show limit]
    pure $ if val == Null
              then Nothing
              else Just $ val .! "."

kmdGetLastNotarisation :: (FromJSON n, Has BitcoinConfig r) => String -> Zeno r (Maybe (Notarisation n))
kmdGetLastNotarisation s = scanNotarisationsDB 0 s 100000

-- | Utils


parseJsonHexSerialized :: Serialize a => Value -> Parser a
parseJsonHexSerialized val = do
  Hex r <- parseJSON val
  either fail pure $ decode r

