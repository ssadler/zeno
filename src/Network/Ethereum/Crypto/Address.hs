
module Network.Ethereum.Crypto.Address where

import           Data.Aeson
import           Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import           Data.String
import qualified Data.Text as T

import           Network.Ethereum.Data.ABI
import           Network.Ethereum.Data

import           Zeno.Prelude


newtype Address = Address { unAddress :: PrefixedHex 20 }
  deriving (Eq, Ord, Serialize, ToJSON, FromJSON, IsString, RLPEncodable)

instance Show Address where
  show = show . unAddress

instance Read Address where
  readsPrec _ s = [(fromString s, "")]

instance GetABI Address where
  getABI = do
    fixed <- getABI
    pure $ Address $ PrefixedHex $ toFixedR (unFixed (fixed :: Bytes32))

instance PutABI Address where
  putABI (Address bs) = do
    let bn = toFixedR $ unFixed $ unPrefixedHex bs
    putABI (bn :: Bytes32)

instance StringConv Address ByteString where
  strConv _ = unFixed . unPrefixedHex . unAddress

nullAddress, maxAddress :: Address
nullAddress = "0x0000000000000000000000000000000000000000"
maxAddress  = "0xFFfFfFffFFfffFFfFFfFFFFFffFFFffffFfFFFfF"
