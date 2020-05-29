{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Crypto.Address where

import           Data.Aeson
import           Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.String

import           Network.Ethereum.Data.ABI
import           Network.Ethereum.Data

import           Zeno.Prelude


newtype Address = Address { fromAddress :: Bytes20 }
  deriving (Eq, Ord, Serialize)

instance Show Address where
  show (Address bs) = "0x" ++ show bs

instance Read Address where
  readsPrec p s =
    if length s == 42 && take 2 s == "0x"
       then let (a, b) = (fromString $ take 20 s, drop 20 s)
             in [(Address a, b)]
       else []

instance IsString Address where
  fromString = read

instance StringConv Address ByteString where
  strConv _ = unFixed . fromAddress

instance FromJSON Address where
  parseJSON val = do
    Hex bs <- parseJSON val
    if BS.length bs == 20
       then pure $ Address $ unsafeToFixed bs
       else fail "Invalid Address"

instance ToJSON Address where
  toJSON (Address bs) = toJSON $ Hex $ unFixed bs

instance PutABI Address where
  putABI (Address bs) =
     putABI bs

instance GetABI Address where
  getABI = Address <$> getABI

instance RLPEncodable Address where
  rlpEncode = rlpEncode . unFixed . fromAddress
  rlpDecode r = do
    s <- rlpDecode r >>= eitherFixed
    pure $ Address s

nullAddress, maxAddress :: Address
nullAddress = "0x0000000000000000000000000000000000000000"
maxAddress  = "0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF"
