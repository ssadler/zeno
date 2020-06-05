{-# LANGUAGE OverloadedStrings #-}

module Zeno.Data.Aeson
  ( module DA
  , SerializeAeson(..)
  , StrictObject
  , withStrictObject
  , (.:-)
  , (.:-?)
  , fromJsonHex
  , toJsonHex
  ) where


import qualified Data.Aeson as Aeson
import           Data.Aeson as DA hiding (encode, decode)
import           Data.Aeson.Types as DA
import           Data.Aeson.Quick as DA (build, (.?), (.!), (.%))
import           Data.Aeson.Encode.Pretty as Pretty
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16 as B16
import           Data.IORef
import           Data.HashMap.Strict
import qualified Data.Serialize as S
import qualified Data.Set as Set
import           Data.Text
import           Data.Text.Encoding
import qualified Data.Serialize as S
import           Zeno.Data.VarInt

import           System.IO.Unsafe


fromJsonHex :: Value -> Parser ByteString
fromJsonHex v = do
  t <- parseJSON v
  case B16.decode (encodeUtf8 t) of (r,"") -> pure r
                                    _      -> fail "Invalid hex data"

toJsonHex :: ByteString -> Value
toJsonHex = String . decodeUtf8 . B16.encode

--------------------------------------------------------------------------------
-- SerializeAeson - A wrapper to provide Serialize via Aeson
--------------------------------------------------------------------------------

newtype SerializeAeson a = SerializeAeson a

instance (ToJSON a, FromJSON a) => S.Serialize (SerializeAeson a) where
  put (SerializeAeson a) = S.put $ VarPrefixedLazyByteString $ encodeStable a
  get = do
    VarPrefixedLazyByteString bs <- S.get
    either fail (pure . SerializeAeson) $ Aeson.eitherDecode bs

encodeStable :: ToJSON a => a -> BSL.ByteString
encodeStable = Pretty.encodePretty' conf
  where conf = Pretty.Config (Pretty.Spaces 0) compare Pretty.Generic False

--------------------------------------------------------------------------------
-- Strict Object - An object where the parse operation must consume all keys
--------------------------------------------------------------------------------

data StrictObject = StrictObject Object (IORef (Set.Set Text))

instance Show StrictObject where
  show (StrictObject o r) = show (o, unsafePerformIO $ readIORef r)


(.:-) :: FromJSON v => StrictObject -> Text -> Parser v
(.:-) so key = addKeyLookup (.:) key so


(.:-?) :: FromJSON v => StrictObject -> Text -> Parser (Maybe v)
(.:-?) so key = addKeyLookup (.:?) key so


addKeyLookup :: (Object -> Text -> Parser v) -> Text -> StrictObject -> Parser v
addKeyLookup op key (StrictObject obj ref) =
  let p = unsafePerformIO $ modifyIORef' ref $ Set.insert key
  in seq p $ op obj key


withStrictObject :: String -> (StrictObject -> Parser a) -> Value -> Parser a
withStrictObject label act = withObject label $ \obj -> do
  let ref = unsafeDepend act $ newIORef Set.empty
  r <- act $ StrictObject obj ref
  let keysRead = unsafeDepend r $ readIORef ref :: Set.Set Text
      objKeys = Set.fromList $ keys obj
      extraKeys = Set.difference objKeys keysRead
  if extraKeys /= mempty
     then let shown = setToString extraKeys
          in fail (label ++ ": extra keys: " ++ shown)
     else pure r
  where
    -- Slightly dodgy function here. It's neccesary to manually add edges
    -- to sequence pure and impure computations in this case.
    unsafeDepend a = unsafePerformIO . seq a
    setToString = unpack . intercalate "," . Set.toList
