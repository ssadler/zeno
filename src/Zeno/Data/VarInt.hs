
module Zeno.Data.VarInt where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Serialize
import Data.Word
import Text.Printf
import qualified Haskoin as H


newtype VarInt = VarInt { unVarInt :: Word64 }
  deriving (Eq, Ord, Num, ToJSON, FromJSON, PrintfArg)
  deriving Serialize via H.VarInt
  deriving Show via Word64


newtype VarPrefixedList a = VarPrefixedList [a]

instance Serialize a => Serialize (VarPrefixedList a) where
  put (VarPrefixedList xs) = do
    put $ VarInt $ fromIntegral $ length xs
    mapM_ put xs
  get = do
    VarInt n <- get
    VarPrefixedList <$> replicateM (fromIntegral n) get


newtype VarPrefixedLazyByteString = VarPrefixedLazyByteString BSL.ByteString

instance Serialize VarPrefixedLazyByteString where
  put (VarPrefixedLazyByteString bs) = do
    put $ VarInt $ fromIntegral $ BSL.length bs
    putLazyByteString bs
  get = do
    n <- fromIntegral . unVarInt <$> get
    VarPrefixedLazyByteString <$> getLazyByteString n
