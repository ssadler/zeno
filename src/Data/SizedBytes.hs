{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Data.SizedBytes
  ( Bytes
  , unBytes
  , bytes
  , bytesGetN
  , bytesReverse
  , nullBytes
  , maxBytes
  , unsafeBytes
  , Bytes0 , Bytes1 , Bytes2 , Bytes3 , Bytes4 , Bytes5 , Bytes6 , Bytes7
  , Bytes8 , Bytes9 , Bytes10 , Bytes11 , Bytes12 , Bytes13 , Bytes14 , Bytes15
  , Bytes16 , Bytes17 , Bytes18 , Bytes19 , Bytes20 , Bytes21 , Bytes22 , Bytes23
  , Bytes24 , Bytes25 , Bytes26 , Bytes27 , Bytes28 , Bytes29 , Bytes30 , Bytes31
  , Bytes32 , Bytes33
  ) where

import           Data.Proxy
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.Word
import           Data.Hashable
import           GHC.TypeLits
import           Data.String
import           Data.Serialize

import           Unsafe.Coerce


-- Bytes type -----------------------------------------------------------------
--
newtype Bytes (n :: Nat) = Bytes { unBytes :: ByteString }
  deriving (Eq, Ord, Hashable)

instance Show (Bytes n) where
  show = BS8.unpack . B16.encode . unBytes

instance forall n. KnownNat n => IsString (Bytes n) where
  fromString = bytes . fromString

instance forall n. KnownNat n => Serialize (Bytes n) where
  put = putByteString . unBytes
  get = Bytes <$> getByteString n where
    n = bytesGetN (Proxy :: Proxy n)

bytes :: forall n. KnownNat n => ByteString -> Bytes n
bytes bs = n `seq` Bytes bs' where
  n = bytesGetN (Proxy :: Proxy n)
  l = BS.length bs
  bs' = case compare l n of
          EQ -> bs
          LT -> bs <> BS.replicate (n-l) 0
          GT -> BS.take n bs

unsafeBytes :: forall n. KnownNat n => ByteString -> Bytes n
unsafeBytes = Bytes

bytesGetN :: forall n. KnownNat n => Proxy n -> Int
bytesGetN = fromIntegral . natVal

bytesReverse :: Bytes n -> Bytes n
bytesReverse (Bytes b) = Bytes $ BS.reverse b

nullBytes :: forall n. KnownNat n => Bytes n
nullBytes = constantBytes 0x00

maxBytes :: forall n. KnownNat n => Bytes n
maxBytes = constantBytes 0xFF

constantBytes :: forall n. KnownNat n => Word8 -> Bytes n
constantBytes = Bytes . BS.replicate (bytesGetN (Proxy :: Proxy n))


type Bytes0  = Bytes 0
type Bytes1  = Bytes 1
type Bytes2  = Bytes 2
type Bytes3  = Bytes 3
type Bytes4  = Bytes 4
type Bytes5  = Bytes 5
type Bytes6  = Bytes 6
type Bytes7  = Bytes 7
type Bytes8  = Bytes 8
type Bytes9  = Bytes 9
type Bytes10 = Bytes 10
type Bytes11 = Bytes 11
type Bytes12 = Bytes 12
type Bytes13 = Bytes 13
type Bytes14 = Bytes 14
type Bytes15 = Bytes 15
type Bytes16 = Bytes 16
type Bytes17 = Bytes 17
type Bytes18 = Bytes 18
type Bytes19 = Bytes 19
type Bytes20 = Bytes 20
type Bytes21 = Bytes 21
type Bytes22 = Bytes 22
type Bytes23 = Bytes 23
type Bytes24 = Bytes 24
type Bytes25 = Bytes 25
type Bytes26 = Bytes 26
type Bytes27 = Bytes 27
type Bytes28 = Bytes 28
type Bytes29 = Bytes 29
type Bytes30 = Bytes 30
type Bytes31 = Bytes 31
type Bytes32 = Bytes 32
type Bytes33 = Bytes 33

