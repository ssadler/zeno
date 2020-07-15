{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Crypto
  ( module ALL
  , EthIdent(..)
  , Address(..)
  , Sha3
  , deriveEthIdent
  , deriveEthAddress
  , recoverAddr
  , sha3'
  , sha3b
  ) where


import           Crypto.Hash
import           Crypto.Secp256k1.Recoverable as ALL

import           Data.ByteString.Short (pack)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.RLP

import           Network.Ethereum.Data.ABI

import           Zeno.Data.Aeson hiding (Key)
import           Zeno.Prelude

import           UnliftIO


data EthIdent = EthIdent 
  { ethSecKey :: SecKey
  , ethAddress :: Address
  } deriving (Show)

deriveEthIdent :: SecKey -> EthIdent
deriveEthIdent sk = EthIdent sk $ deriveEthAddress $ derivePubKey sk

deriveEthAddress :: PubKey -> Address
deriveEthAddress pk = Address . toFixedR . sha3' . BS.drop 1 $ exportPubKey False pk

recoverAddr :: Fixed 32 s => s -> RecSig -> Either String Address
recoverAddr bs crs = do
  recover crs bs <&> deriveEthAddress



-- | It would be neat if this held a plain bytestring and the module didn't import
--   the Address constructor
newtype Address = Address { unAddress :: Bytes20 }
  deriving (Eq, Ord, RLPEncodable, Read, Show, Serialize, ToJSON, FromJSON, IsString, Bounded)
       via (PrefixedHash 20)

-- There could be a FixedBytesR for this
instance GetABI Address where
  getABI = do
    fixed <- getABI
    pure $ Address $ toFixedR $ fromFixed (fixed :: Bytes32)

instance PutABI Address where
  putABI (Address bs) = do
    let bn = toFixedR $ fromFixed bs
    putABI (bn :: Bytes32)

instance StringConv Address ByteString where
  strConv _ = fromFixed . unAddress



type Sha3 = PrefixedHash 32


sha3' :: ByteString -> ByteString
sha3' bs = BS.pack (BA.unpack (hash bs :: Digest Keccak_256))

sha3 :: ByteString -> Sha3
sha3 = toFixed . sha3'

sha3b :: ByteString -> Bytes32
sha3b bs = unsafeToFixed $ pack (BA.unpack (hash bs :: Digest Keccak_256))

