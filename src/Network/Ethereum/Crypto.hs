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


import           Crypto.Secp256k1.Recoverable as ALL

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
deriveEthIdent sk = EthIdent sk <$> (deriveEthAddress =<< derivePubKeyIO sk)

deriveEthAddress :: MonadUnliftIO m => PubKey -> m Address
deriveEthAddress pk = Address . toFixedR . sha3' . BS.drop 1 <$> exportPubKeyIO False pk

recoverAddr :: (MonadUnliftIO m, Fixed 32 s) => s -> RecSig -> m Address
recoverAddr bs crs = do
  recoverIO crs bs >>= deriveEthAddress
