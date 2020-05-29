{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Network.Ethereum.Crypto.Hash
  ( Sha3(..)
  , sha3
  , sha3'
  , sha3b
  , nullSha3
  , sha3AsBytes32
  ) where

import           Crypto.Hash

import           Data.Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.Serialize

import           Network.Ethereum.Data
import           Zeno.Prelude
import           Zeno.Data.FixedBytes


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
  putABI (Sha3 bs) = putABI (unsafeToFixed bs :: Bytes32)

instance GetABI Sha3 where
  getABI = do
    b <- getABI
    pure $ Sha3 $ unFixed (b :: Bytes32)

instance Serialize Sha3 where
  put (Sha3 bs) = putByteString bs
  get = Sha3 <$> getByteString 32

sha3AsBytes32 :: Sha3 -> Bytes32
sha3AsBytes32 (Sha3 b) = unsafeToFixed b

failableSha3 :: (Monad m, MonadFail m) => ByteString -> m Sha3
failableSha3 bs = 
  if BS.length bs == 32
     then pure $ Sha3 bs
     else fail "malformed hash"

sha3' :: ByteString -> ByteString
sha3' bs = BS.pack (BA.unpack (hash bs :: Digest Keccak_256))

sha3 :: ByteString -> Sha3
sha3 = Sha3 . sha3'

sha3b :: ByteString -> Bytes32
sha3b = unsafeToFixed . sha3'


  
nullSha3 :: Sha3
nullSha3 = "0x0000000000000000000000000000000000000000000000000000000000000000"

