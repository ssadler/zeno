{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Crypto
  ( module ALL
  , EthIdent(..)
  , Address(..)
  , CompactRecSig(..)
  , Key(..)
  , deriveEthIdent
  , pubKeyAddr
  , genSecKey
  , sign
  , recoverAddr
  , hashMsg
  ) where


import           Crypto.Secp256k1 as ALL
                 (Msg, PubKey, SecKey, msg, secKey, getMsg, derivePubKey)
import qualified Crypto.Secp256k1 as Secp256k1

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Binary
import           Data.Monoid

import           Network.Ethereum.Crypto.Address as ALL
import           Network.Ethereum.Crypto.Hash as ALL
import           Network.Ethereum.Crypto.Trie as ALL
import           Zeno.Data.Aeson hiding (Key)
import           Zeno.Prelude
import           Zeno.Data.Hex

import           System.Entropy


data EthIdent = EthIdent SecKey Address

deriveEthIdent :: SecKey -> EthIdent
deriveEthIdent sk = EthIdent sk $ pubKeyAddr $ derivePubKey sk

pubKeyAddr :: PubKey -> Address
pubKeyAddr = Address . BS.drop 12 . sha3' . BS.drop 1 . Secp256k1.exportPubKey False

recoverAddr :: Msg -> CompactRecSig -> Maybe Address
recoverAddr msg crs = pubKeyAddr <$> recover crs msg

genSecKey :: IO SecKey
genSecKey = do
  bytes <- getEntropy 32
  case secKey bytes of
       Just sk -> pure sk
       Nothing -> fail "IO error generating secret key"


-- TODO: Return Either here
recover :: CompactRecSig -> Msg -> Maybe PubKey
recover crs message = do
  rs <- Secp256k1.importCompactRecSig $ toLegacyCRS crs
  let s = Secp256k1.convertRecSig rs
      (_, bad) = Secp256k1.normalizeSig s
   in if bad then Nothing
             else Secp256k1.recover rs message


hashMsg :: ByteString -> Msg
hashMsg = fromJust . msg . sha3'


data CompactRecSig = CompactRecSig
  { sigR :: ShortByteString
  , sigS :: ShortByteString
  , sigV :: Word8
  } deriving (Eq, Ord, Show)

toLegacyCRS :: CompactRecSig -> Secp256k1.CompactRecSig
toLegacyCRS (CompactRecSig r s v) = Secp256k1.CompactRecSig s r v

fromLegacyCRS :: Secp256k1.CompactRecSig -> CompactRecSig
fromLegacyCRS (Secp256k1.CompactRecSig s r v) = CompactRecSig r s v

sign :: SecKey -> Msg -> CompactRecSig
sign sk msg = fromLegacyCRS $ Secp256k1.exportCompactRecSig $ Secp256k1.signRecMsg sk msg


instance ToJSON CompactRecSig where
  toJSON (CompactRecSig r s v) = toJsonHex $
    fromShort r <> fromShort s <> (if v == 0 then "\0" else "\1")

instance FromJSON CompactRecSig where
  parseJSON val = do
    bs <- unHex <$> parseJSON val
    let (r, rest) = BS8.splitAt 32 bs
        (s, v)    = BS8.splitAt 32 rest
        f = pure . CompactRecSig (toShort r) (toShort s)
    case v of "\0" -> f 0
              "\1" -> f 1
              _      -> fail "Sig invalid"

instance Binary CompactRecSig where
  put (CompactRecSig r s v) = put r >> put s >> put v
  get = CompactRecSig <$> get <*> get <*> get



newtype Key a = Key a

instance (Show a) => ToJSON (Key a) where
  toJSON (Key a) = toJSON $ init $ drop 8 $ show a
