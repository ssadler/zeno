{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- Bytes type
--
-- Newtype container for friendly and safe storage of fixed size
-- byte array. This container will always contain a byte array with
-- the size specified in the type, unless the `unsafeBytes` constructor
-- is used incorrectly. String functions use hexidecimal representation.

module Data.FixedBytes
  ( module Out
  , FixedBytes
  , bytesReverse
  , eitherFixed
  , prefixedFromHex
  , newFixed
  , nullBytes
  , fixedGetN
  , toFixed
  , toFixedR
  , unsafeToFixed
  , unFixed
  , bappend
  , reFixed
  , Bytes0 , Bytes1 , Bytes2 , Bytes3 , Bytes4 , Bytes5 , Bytes6 , Bytes7
  , Bytes8 , Bytes9 , Bytes10 , Bytes11 , Bytes12 , Bytes13 , Bytes14 , Bytes15
  , Bytes16 , Bytes17 , Bytes18 , Bytes19 , Bytes20 , Bytes21 , Bytes22 , Bytes23
  , Bytes24 , Bytes25 , Bytes26 , Bytes27 , Bytes28 , Bytes29 , Bytes30 , Bytes31
  , Bytes32 , Bytes33
  -- Hex Prefixed version
  , PrefixedHex(..)
  ) where


import           Data.Aeson
import           Data.Proxy as Out (Proxy(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.String.Conv
import qualified Data.Text as T
import           Data.Word
import           Data.Hashable
import           Data.String
import           Data.Serialize
import qualified Data.RLP as RLP
import           GHC.TypeLits
import           Text.Printf


-- Bytes type -----------------------------------------------------------------
--
newtype FixedBytes (n :: Nat) = Bytes { unFixed :: ByteString }
  deriving (Eq, Ord, Hashable)

instance Show (FixedBytes n) where
  show = BS8.unpack . B16.encode . unFixed

instance forall n. KnownNat n => Read (FixedBytes n) where
  readsPrec _ s =
    case bytesFromHex (toS s) of
      Left _ -> []
      Right b -> [(b, "")]

instance forall n. KnownNat n => IsString (FixedBytes n) where
  fromString s =
    case bytesFromHex (BS8.pack s) of
      Left s -> error s
      Right o -> o

instance forall n. KnownNat n => Serialize (FixedBytes n) where
  put = putByteString . unFixed
  get = Bytes <$> getByteString n where
    n = fixedGetN (Proxy :: Proxy n)

instance forall n. KnownNat n => ToJSON (FixedBytes n) where
  toJSON = toJSON . BS8.unpack . B16.encode . unFixed

instance forall n. KnownNat n => FromJSON (FixedBytes n) where
  parseJSON val = parseJSON val >>= either fail pure . bytesFromHex . BS8.pack

instance forall n. KnownNat n => StringConv (FixedBytes n) (FixedBytes n) where
  strConv _ = id

instance forall n. KnownNat n => PrintfArg (FixedBytes n) where
  formatArg = formatArg . BS8.unpack . B16.encode . unFixed

instance forall n. KnownNat n => RLP.RLPEncodable (FixedBytes n) where
  rlpEncode = RLP.rlpEncode . unFixed
  rlpDecode r = do
    RLP.rlpDecode r >>= eitherFixed

instance forall n. KnownNat n => Bounded (FixedBytes n) where
  minBound = newFixed 0
  maxBound = newFixed 255

bappend :: forall n m. (KnownNat n, KnownNat m)
        => FixedBytes n -> FixedBytes m -> FixedBytes (n + m)
bappend (Bytes b) (Bytes b') = Bytes (b <> b')

toFixed :: forall n. KnownNat n => ByteString -> FixedBytes n
toFixed bs = n `seq` Bytes bs' where
  n = fixedGetN (Proxy :: Proxy n)
  l = BS.length bs
  bs' = case compare l n of
          EQ -> bs
          LT -> bs <> BS.replicate (n-l) 0
          GT -> BS.take n bs

toFixedR :: forall n. KnownNat n => ByteString -> FixedBytes n
toFixedR bs = n `seq` Bytes bs' where
  n = fixedGetN (Proxy :: Proxy n)
  l = BS.length bs
  bs' = case compare l n of
          EQ -> bs
          LT -> BS.replicate (n-l) 0 <> bs
          GT -> BS.drop (max 0 (l-n)) bs

unsafeToFixed :: forall n. KnownNat n => ByteString -> FixedBytes n
unsafeToFixed = Bytes
{-# INLINE unsafeToFixed #-}

fixedGetN :: forall n. KnownNat n => Proxy n -> Int
fixedGetN = fromIntegral . natVal

bytesReverse :: FixedBytes n -> FixedBytes n
bytesReverse (Bytes b) = Bytes $ BS.reverse b

bytesFromHex :: forall n. KnownNat n => ByteString -> Either String (FixedBytes n)
bytesFromHex bsHex
  | r /= ""           = Left $ "Invalid hex: " ++ show bsHex
  | BS.length bs /= n = Left "Incorrect length"
  | otherwise         = Right $ Bytes bs
  where
  n = fixedGetN (Proxy :: Proxy n)
  (bs, r) = B16.decode bsHex

nullBytes :: forall n. KnownNat n => FixedBytes n
nullBytes = newFixed 0x00

newFixed :: forall n. KnownNat n => Word8 -> FixedBytes n
newFixed = Bytes . BS.replicate (fixedGetN (Proxy :: Proxy n))

eitherFixed :: forall n. KnownNat n => ByteString -> Either String (FixedBytes n)
eitherFixed bs
  | n == l = Right $ Bytes bs
  | otherwise = Left $ "Incorrect length: " ++ show l
  where
  n = fixedGetN (Proxy :: Proxy n)
  l = BS.length bs

reFixed :: (KnownNat n, KnownNat m) => FixedBytes n -> FixedBytes m
reFixed = toFixed . unFixed


type Bytes0  = FixedBytes 0
type Bytes1  = FixedBytes 1
type Bytes2  = FixedBytes 2
type Bytes3  = FixedBytes 3
type Bytes4  = FixedBytes 4
type Bytes5  = FixedBytes 5
type Bytes6  = FixedBytes 6
type Bytes7  = FixedBytes 7
type Bytes8  = FixedBytes 8
type Bytes9  = FixedBytes 9
type Bytes10 = FixedBytes 10
type Bytes11 = FixedBytes 11
type Bytes12 = FixedBytes 12
type Bytes13 = FixedBytes 13
type Bytes14 = FixedBytes 14
type Bytes15 = FixedBytes 15
type Bytes16 = FixedBytes 16
type Bytes17 = FixedBytes 17
type Bytes18 = FixedBytes 18
type Bytes19 = FixedBytes 19
type Bytes20 = FixedBytes 20
type Bytes21 = FixedBytes 21
type Bytes22 = FixedBytes 22
type Bytes23 = FixedBytes 23
type Bytes24 = FixedBytes 24
type Bytes25 = FixedBytes 25
type Bytes26 = FixedBytes 26
type Bytes27 = FixedBytes 27
type Bytes28 = FixedBytes 28
type Bytes29 = FixedBytes 29
type Bytes30 = FixedBytes 30
type Bytes31 = FixedBytes 31
type Bytes32 = FixedBytes 32
type Bytes33 = FixedBytes 33



prefixedFromHex :: forall n. KnownNat n => ByteString -> Either String (PrefixedHex n)
prefixedFromHex bs =
  let n = if BS.take 2 bs == "0x" then 2 else 0
   in PrefixedHex <$> bytesFromHex (BS.drop n bs)


newtype PrefixedHex n = PrefixedHex { unPrefixedHex :: FixedBytes n }
  deriving (Eq, Ord, Serialize, Bounded, RLP.RLPEncodable)

instance Show (PrefixedHex n) where
  show (PrefixedHex a) = "0x" ++ show a

instance forall n. KnownNat n => Read (PrefixedHex n) where
  readsPrec _ s =
    case prefixedFromHex (toS s) of
      Left _ -> []
      Right o -> [(o, "")]

instance forall n. KnownNat n => FromJSON (PrefixedHex n) where
  parseJSON val = do
    String s <- parseJSON val
    PrefixedHex <$> 
      case T.take 2 s of
        "0x" -> parseJSON (String (T.drop 2 s))
        _ -> parseJSON val

instance forall n. KnownNat n => ToJSON (PrefixedHex n) where
  toJSON (PrefixedHex val) =
    let String s = toJSON val
     in String $ "0x" <> s

instance forall n. KnownNat n => IsString (PrefixedHex n) where
  fromString s =
    let s' = if take 2 s == "0x" then drop 2 s else s
     in PrefixedHex $ fromString s'

instance forall n. KnownNat n => StringConv (PrefixedHex n) (FixedBytes n) where
  strConv _ = unPrefixedHex
