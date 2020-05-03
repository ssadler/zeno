{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}

module Network.Ethereum.Crypto.Hash
  ( Sha3(..)
  , sha3
  , sha3'
  ) where

import           Crypto.Hash

import           Data.Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.Serialize

import           Network.Ethereum.Data
import           Zeno.Prelude


newtype Sha3 = Sha3 { unSha3 :: ByteString }
  deriving (Eq, Ord, RLPEncodable)

instance Show Sha3 where
  show (Sha3 bs) = asString (toHex bs)

instance Read Sha3 where
  readsPrec a s
    | take 2 s == "0x" = readsPrec a $ drop 2 s
    | length s == 64   = [(Sha3 $ fromHex $ fromString s, "")]
    | otherwise        = []

instance IsString Sha3 where
  fromString s = read s

instance FromJSON Sha3 where
  parseJSON val = do
    Hex bs <- parseJSON val
    failableSha3 bs

instance ToJSON Sha3 where
  toJSON = toJSON . Hex . unSha3

instance PutABI Sha3 where
  putABI (Sha3 bs) = putABI (bytes bs :: Bytes 32)

instance Serialize Sha3 where
  put (Sha3 bs) = putByteString bs
  get = Sha3 <$> getByteString 32

failableSha3 :: (Monad m, MonadFail m) => ByteString -> m Sha3
failableSha3 bs = 
  if BS.length bs == 32
     then pure $ Sha3 bs
     else fail "malformed hash"

sha3' :: ByteString -> ByteString
sha3' bs = BS.pack (BA.unpack (hash bs :: Digest Keccak_256))

sha3 :: ByteString -> Sha3
sha3 = Sha3 . sha3'
