{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Komodo where

import           Crypto.Secp256k1 as EC
import           Data.Serialize as S

import           Network.Bitcoin
import           Network.Haskoin.Crypto as H
import           Network.Haskoin.Keys as H
import           Network.Haskoin.Transaction as H
import           Network.Haskoin.Constants as H
import           Network.Haskoin.Address as H

import           Zeno.Prelude
import           Zeno.Data.Aeson
import           Zeno.Data.Hex
import qualified Zeno.Data.Binary as Bin


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

instance Bin.Binary RAddress where
  put = Bin.put . Bin.Ser2Bin . getAddrHash
  get = RAddress . Bin.unSer2Bin <$> Bin.get

instance IsString RAddress where
  fromString s = maybe (error e) id $ stringToRAddress s
    where e = "Invalid Komodo address: " ++ show s

stringToRAddress :: String -> Maybe RAddress
stringToRAddress s =
  case H.stringToAddr komodo (fromString s) of
    Just (H.PubKeyAddress h160) -> Just (RAddress h160)
    _ -> Nothing



-- Ident ----------------------------------------------------------------------

type KomodoIdent = (H.SecKeyI, H.PubKeyI, RAddress)

deriveKomodoIdent :: SecKey -> KomodoIdent
deriveKomodoIdent sk =
  let ski = H.SecKeyI sk True
      pubKey = H.derivePubKeyI ski
   in (ski, pubKey, RAddress $ H.addressHash $ S.encode pubKey)


-- UTXOs ----------------------------------------------------------------------

komodoUtxos :: Has BitcoinConfig r => [RAddress] -> Zeno r [KomodoUtxo]
komodoUtxos addrs = queryBitcoin "listunspent" (1::Int, 99999999::Int, addrs)

data KomodoUtxo = Utxo
  { utxoAmount :: Word64
  , utxoConfirmations :: Int
  , utxoTxid :: H.TxHash
  , utxoVout :: Word32
  , utxoAddress :: RAddress
  , utxoSpendable :: Bool
  } deriving (Show)

instance FromJSON KomodoUtxo where
  parseJSON val = do
    obj <- parseJSON val
    amount <- obj .: "amount"
    Utxo (floor $ amount * (1e8::Scientific))
         <$> obj .: "confirmations"
         <*> obj .: "txid"
         <*> obj .: "vout"
         <*> obj .: "address"
         <*> obj .: "spendable"

getOutPoint :: KomodoUtxo -> H.OutPoint
getOutPoint utxo = H.OutPoint (utxoTxid utxo) (utxoVout utxo)





-- Notarisation Data ----------------------------------------------------------
--
-- (Doesn't support backnotarisation yet)

data NotarisationData h = NOR
  { blockHash :: h
  , blockNumber :: Word32
  , symbol :: String
  , mom :: h
  , momDepth  :: Word16
  , ccId :: Word16
  } deriving (Eq, Show)

momDepth32 :: Integral a => NotarisationData h -> a
momDepth32 = fromIntegral . momDepth

instance Serialize h => Serialize (NotarisationData h) where
  put NOR{..} = do
    put blockHash >> putWord32le blockNumber
    mapM put symbol >> put '\0'
    put mom >> putWord16le momDepth >> putWord16le ccId
  get = do
    let getSymbol =
          get >>= \case '\0' -> pure ""
                        i    -> (i:) <$> getSymbol
    NOR <$> get <*> getWord32le <*> getSymbol
        <*> get <*> getWord16le <*> getWord16le

instance Serialize h => Bin.Binary (NotarisationData h) where
  put = Bin.put . Bin.Ser2Bin
  get = Bin.unSer2Bin <$> Bin.get

instance Serialize h => FromJSON (NotarisationData h) where
  parseJSON val = do
    Hex bs <- parseJSON val
    either fail pure $ decode bs

-- Notarisation RPC

data Notarisation h = Notarisation
  { kmdHeight :: Word32
  , txHash :: TxHash
  , opret :: NotarisationData h
  } deriving (Show)

instance Serialize h => FromJSON (Notarisation h) where
  parseJSON val = do
    obj <- parseJSON val
    Notarisation <$> obj .: "height"
                 <*> obj .: "hash"
                 <*> obj .: "opreturn"

scanNotarisationsDB :: (Serialize h, Has BitcoinConfig r) => Word32 -> String ->
                    Word32 -> Zeno r (Maybe (Notarisation h))
scanNotarisationsDB height symbol limit = do
  traceE "scanNotarisationsDB" $ do
    val <- queryBitcoin "scanNotarisationsDB" [show height, symbol, show limit]
    pure $ if val == Null
              then Nothing
              else Just $ val .! "."

getLastNotarisation :: (Serialize h, Has BitcoinConfig r) => String ->
                       Zeno r (Maybe (Notarisation h))
getLastNotarisation s = scanNotarisationsDB 0 s 10000

