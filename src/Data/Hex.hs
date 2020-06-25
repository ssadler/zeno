{-# LANGUAGE TypeApplications #-}

module Data.Hex
  ( Hex(..)
  , PrefixedHex(..)
  , fromHex
  , toHex
  ) where


import           Data.Aeson.Types (Parser, ToJSON(..), FromJSON(..), Value(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16
import           Data.Serialize
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Typeable ((:~:)(..), Typeable, cast, eqT)

type Hexable a = (Serialize a, Typeable a)

newtype Hex a = Hex { unHex :: a }
  deriving (Eq, Ord)

instance Hexable a => Show (Hex a) where
  show = T.unpack . encodeHex

instance Hexable a => ToJSON (Hex a) where
  toJSON = toJSON . encodeHex

newtype PrefixedHex a = PrefixedHex { unPrefixedHex :: a }
  deriving (Eq, Ord)
  deriving (FromJSON, Read) via (Hex a)

instance Hexable a => Show (PrefixedHex a) where
  show (PrefixedHex a) = T.unpack $ "0x" <> encodeHex (Hex a)

instance Hexable a => Read (Hex a) where
  readsPrec a s
    | take 2 s == "0x" = readsPrec a $ drop 2 s
    | otherwise = either (\_ -> []) (\a -> [(Hex a, "")]) $ fromBS $ fromHex $ BS.pack s

instance Hexable a => IsString (Hex a) where
  fromString s = read s

instance Hexable a => ToJSON (PrefixedHex a) where
  toJSON (PrefixedHex o) = String $ "0x" <> encodeHex (Hex o)

instance Hexable a => FromJSON (Hex a) where
  parseJSON val = do
    pt <- parseJSON val
    let t = maybe pt id $ T.stripPrefix "0x" pt
    bs <- case B16.decode (encodeUtf8 t) of
            (r, "") -> pure r
            _       -> fail "Invalid hex data"
    either fail (pure . Hex) $ fromBS bs

encodeHex :: Hexable a => Hex a -> T.Text
encodeHex (Hex o) = decodeUtf8 $ B16.encode $ maybe (encode o) id $ cast o

fromBS :: forall a. Hexable a => BS.ByteString -> Either String a
fromBS bs =
  case eqT @a @BS.ByteString of
    Just Refl -> pure bs
    Nothing -> decode bs


fromHex :: BS.ByteString -> BS.ByteString
fromHex bs =
  if BS.take 2 bs == "0x"
     then fromHex $ BS.drop 2 bs
     else let (b,r) = B16.decode bs
           in if r /= "" then error "Invalid hex" else b

toHex :: BS.ByteString -> BS.ByteString
toHex = B16.encode
