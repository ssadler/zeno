{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Zeno.Data.Hex
  ( Hex(..)
  , Bytes(..)
  , bytes
  , bytesGetN
  , bytesReverse
  , nullBytes
  , maxBytes
  ) where


import           GHC.TypeLits

import qualified Data.ByteString as BS
import           Data.Char
import           Data.Proxy
import           Data.Serialize
import qualified Data.Text as T

import           Zeno.Data.Aeson
import           Zeno.Prelude


newtype Hex = Hex { unHex :: ByteString }
  deriving (Eq, Ord)

instance Show Hex where
  show (Hex bs) = show $ "0x" <> toHex bs

instance Read Hex where
  readsPrec a s
    | take 2 s == "0x" = readsPrec a $ drop 2 s
    | otherwise        = [(Hex $ fromHex $ fromString s, "")]

instance IsString Hex where
  fromString s = read s

instance ToJSON Hex where
  toJSON (Hex bs) = String $ decodeUtf8 $ "0x" <> toHex bs
  {-# INLINABLE toJSON #-}

instance FromJSON Hex where
  parseJSON val = do
    s <- parseJSON val
    let r = if T.take 2 s == "0x" then T.drop 2 s else s
     in Hex <$> fromJsonHex (String r)
  {-# INLINABLE parseJSON #-}


-- Bytes type -----------------------------------------------------------------
--
newtype Bytes (n :: Nat) = Bytes { unBytes :: ByteString }
  deriving (Eq, Ord)

instance Show (Bytes n) where
  show = show . asString . unBytes

instance forall n. KnownNat n => IsString (Bytes n) where
  fromString = bytes . fromString

instance forall n. KnownNat n => Serialize (Bytes n) where
  put = putByteString . unBytes
  get = 
    let n = bytesGetN (Proxy :: Proxy n)
     in Bytes <$> getByteString n


bytes :: forall n. KnownNat n => ByteString -> Bytes n
bytes =
  let n = bytesGetN (Proxy :: Proxy n)
   in n `seq` Bytes

bytesGetN :: forall n. KnownNat n => Proxy n -> Int
bytesGetN = fromIntegral . natVal

bytesReverse :: Bytes n -> Bytes n
bytesReverse (Bytes b) = Bytes (BS.reverse b)

nullBytes :: forall n. KnownNat n => Bytes n
nullBytes = constantBytes 0x00

maxBytes :: forall n. KnownNat n => Bytes n
maxBytes = constantBytes 0xFF

constantBytes :: forall n. KnownNat n => Word8 -> Bytes n
constantBytes = Bytes . BS.replicate (bytesGetN (Proxy :: Proxy n))
