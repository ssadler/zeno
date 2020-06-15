{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.Secp256k1.Recoverable
  ( PubKey(unPubKey)
  , RecSig(unRecSig)
  , SecKey(unSecKey)
  , derivePubKeyIO
  , exportPubKeyIO
  , signIO
  , recoverIO
  , fromRSV
  , toRSV
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Short
import Data.ByteArray (ByteArrayAccess, withByteArray, alloc)
import Data.FixedBytes
import Data.String
import Data.Word
import Data.ByteString (ByteString)
import Foreign
import Foreign.C.Types
import Data.Serialize


newtype PubKey = PubKey { unPubKey :: FixedBytes 33 }
  deriving (Eq, Read, Show, Serialize, ByteArrayAccess, IsString, Fixed 33) via FixedBytes 33
newtype SecKey = SecKey { unSecKey :: FixedBytes 32 }
  deriving (Eq, Read, Show, Serialize, ByteArrayAccess, IsString, Fixed 32) via FixedBytes 32
newtype RecSig = RecSig { unRecSig :: FixedBytes 65 }
  deriving (Eq, Read, Show, Serialize, ByteArrayAccess, FromJSON, ToJSON, IsString, Fixed 65) via FixedBytes 65

toRSV :: RecSig -> (Bytes32, Bytes32, Word8)
toRSV (RecSig b) =
  let (r, sv) = splitFixed b
      (s, bv) = splitFixed sv
      [v] = unpack $ unFixed bv
   in (bytesReverse r, bytesReverse s, v)

fromRSV :: Bytes32 -> Bytes32 -> Word8 -> RecSig
fromRSV r s v = RecSig $ r `bappend` s `bappend` newFixed v

foreign import ccall unsafe "secp256k1_recoverable_sign"
  c_secp256k1_recoverable_sign :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

signIO :: (Fixed 32 msg, MonadIO m) => SecKey -> msg -> m RecSig
signIO sk msg =
  liftIO do
    fmap toFixed do
      withByteArray sk \psk -> do
        withByteArray (asFixed msg) $ \pmsg -> do
          alloc 65 \psig -> do
            ret <- c_secp256k1_recoverable_sign psig pmsg psk
            unless (ret == 1) $ error $ "sign returned: " ++ show ret

foreign import ccall unsafe "secp256k1_recoverable_recover"
  c_secp256k1_recoverable_recover :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

recoverIO :: (Fixed 32 msg, MonadIO m) => RecSig -> msg -> m PubKey
recoverIO sig msg =
  liftIO do
    fmap toFixed do
      withByteArray sig \psig -> do
        withByteArray (asFixed msg) $ \pmsg -> do
          alloc 33 \ppk -> do
            ret <- c_secp256k1_recoverable_recover ppk psig pmsg
            unless (ret == 1) $ error $ "recover returned: " ++ show ret

foreign import ccall unsafe "secp256k1_recoverable_derive_pubkey"
  c_secp256k1_recoverable_derive_pubkey :: Ptr Word8 -> Ptr Word8 -> IO CInt

derivePubKeyIO :: MonadIO m => SecKey -> m PubKey
derivePubKeyIO sk =
  liftIO do
    fmap toFixed do
      withByteArray sk \psk -> do
        alloc 33 \ppk -> do
          ret <- c_secp256k1_recoverable_derive_pubkey ppk psk
          unless (ret == 1) $ error $ "derive returned: " ++ show ret

foreign import ccall unsafe "secp256k1_recoverable_pubkey_serialize_der"
  c_secp256k1_recoverable_pubkey_serialize_der :: Ptr Word8 -> Ptr Word8 -> IO CInt

exportPubKeyIO :: MonadIO m => Bool -> PubKey -> m ByteString
exportPubKeyIO True pk = pure $ fromFixed pk
exportPubKeyIO False pk = do
  liftIO do
    withByteArray pk \ppk -> do
      alloc 65 \pout -> do
        ret <- c_secp256k1_recoverable_pubkey_serialize_der pout ppk
        unless (ret == 1) $ error $ "derive returned: " ++ show ret
