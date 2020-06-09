{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Crypto
  ( module ALL
  , EthIdent(..)
  , Address(..)
  , deriveEthIdent
  , deriveEthAddress
  , recoverAddr
  ) where


import           Crypto.Secp256k1Wrapped as ALL

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import           Network.Ethereum.Crypto.Address as ALL
import           Network.Ethereum.Crypto.Hash as ALL
import           Zeno.Data.Aeson hiding (Key)
import           Zeno.Prelude
import           Zeno.Data.Hex

import           UnliftIO

import           System.Entropy


data EthIdent = EthIdent 
  { ethSecKey :: SecKey
  , ethAddress :: Address
  } deriving (Show)

deriveEthIdent :: MonadUnliftIO m => SecKey -> m EthIdent
deriveEthIdent sk = EthIdent sk <$> (deriveEthAddress =<< derivePubKey sk)

deriveEthAddress :: MonadUnliftIO m => PubKey -> m Address
deriveEthAddress pk = Address . toFixedR . sha3' . BS.drop 1 <$> exportPubKey False pk

recoverAddr :: (MonadUnliftIO m, ToBytes32 s) => s -> CompactRecSig -> m (Either String Address)
recoverAddr bs crs = do
  recover crs bs >>= mapM deriveEthAddress


instance ToJSON CompactRecSig where
  toJSON (CompactRecSig r s v) = toJsonHex $
    fromShort r <> fromShort s <> (if v == 0 then "\0" else "\1")

instance FromJSON CompactRecSig where
  parseJSON val = do
    bs <- unHex <$> parseJSON val
    let (r, rest) = BS8.splitAt 32 bs
        (s, v)    = BS8.splitAt 32 rest
        f = pure . CompactRecSig (toShort r) (toShort s)
    case v of "\0" -> f 0
              "\1" -> f 1
              _      -> fail "Sig invalid"
