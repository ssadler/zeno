{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Crypto.Secp256k1.Recoverable
  ( PubKey(unPubKey)
  , RecSig(unRecSig)
  , SecKey(unSecKey)
  , derivePubKey
  , exportPubKey
  , sign
  , recover
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
import Lens.Micro.Platform
import System.IO.Unsafe
import Test.QuickCheck (Arbitrary)


newtype PubKey = PubKey { unPubKey :: FixedBytes 33 }
  deriving (Eq, Read, Show, Serialize, ByteArrayAccess, IsString, Fixed 33) via FixedBytes 33
newtype SecKey = SecKey { unSecKey :: FixedBytes 32 }
  deriving (Eq, Read, Show, Serialize, ByteArrayAccess, IsString, Bounded, Enum, Fixed 32) via FixedBytes 32
newtype RecSig = RecSig { unRecSig :: FixedBytes 65 }
  deriving ( Eq, Read, Show, Serialize, IsString, FromJSON, ToJSON
           , ByteArrayAccess, Arbitrary, Fixed 65
           ) via FixedBytes 65

toRSV :: RecSig -> (Integer, Integer, Word8)
toRSV b =
  let bs = unpack $ unFixed b
      (r, (s, [v])) = over _2 (splitAt 32) (splitAt 32 bs)
   in (intFromBytesLE r, intFromBytesLE s, fromIntegral v)

fromRSV :: (Integer, Integer, Word8) -> RecSig
fromRSV (r, s, v) = RecSig $ f r `bappend` f s `bappend` newFixed v
  where f = toFixed . BS.pack . intToBytesLE :: Integer -> Bytes32

foreign import ccall unsafe "secp256k1_recoverable_sign"
  c_secp256k1_recoverable_sign :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

sign :: Fixed 32 msg => SecKey -> msg -> RecSig
sign sk msg =
  unsafePerformIO do
    fmap toFixed do
      withByteArray sk \psk -> do
        withByteArray (asFixed msg) $ \pmsg -> do
          alloc 65 \psig -> do
            ret <- c_secp256k1_recoverable_sign psig pmsg psk
            unless (ret == 1) $ error $ "sign returned: " ++ show ret

foreign import ccall unsafe "secp256k1_recoverable_recover"
  c_secp256k1_recoverable_recover :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO CInt

recover :: Fixed 32 msg => RecSig -> msg -> Either String PubKey
recover sig msg =
  unsafePerformIO do
    withByteArray sig \psig -> do
      withByteArray (asFixed msg) $ \pmsg -> do
        ba <- alloc 33 mempty
        withByteArray ba \ppk -> do
          ret <- c_secp256k1_recoverable_recover ppk psig pmsg
          case ret of
            1 -> pure $ Right $ toFixed ba
            n -> pure $ Left $ "recover returned: " ++ show ret

foreign import ccall unsafe "secp256k1_recoverable_derive_pubkey"
  c_secp256k1_recoverable_derive_pubkey :: Ptr Word8 -> Ptr Word8 -> IO CInt

derivePubKey :: SecKey -> PubKey
derivePubKey sk =
  unsafePerformIO do
    fmap toFixed do
      withByteArray sk \psk -> do
        alloc 33 \ppk -> do
          ret <- c_secp256k1_recoverable_derive_pubkey ppk psk
          unless (ret == 1) $ error $ "derive returned: " ++ show ret

foreign import ccall unsafe "secp256k1_recoverable_pubkey_serialize_der"
  c_secp256k1_recoverable_pubkey_serialize_der :: Ptr Word8 -> Ptr Word8 -> IO CInt

exportPubKey :: Bool -> PubKey -> ByteString
exportPubKey True pk = fromFixed pk
exportPubKey False pk = do
  unsafePerformIO do
    withByteArray pk \ppk -> do
      alloc 65 \pout -> do
        ret <- c_secp256k1_recoverable_pubkey_serialize_der pout ppk
        unless (ret == 1) $ error $ "export returned: " ++ show ret
