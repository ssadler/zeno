
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


-- | It would be neat if this held a plain bytestring and the module didn't import
--   the Address constructor
newtype Address = Address { unAddress :: Bytes20 }
  deriving (Eq, Ord, RLPEncodable, Read, Show, Serialize, ToJSON, FromJSON, IsString, Bounded)
       via (PrefixedHex 20)

-- There could be a FixedBytesR for this
instance GetABI Address where
  getABI = do
    fixed <- getABI
    pure $ Address $ toFixedR (unFixed (fixed :: Bytes32))

instance PutABI Address where
  putABI (Address bs) = do
    let bn = toFixedR $ unFixed bs
    putABI (bn :: Bytes32)

instance StringConv Address ByteString where
  strConv _ = unFixed . unAddress
