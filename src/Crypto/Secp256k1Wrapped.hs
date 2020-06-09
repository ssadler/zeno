
module Crypto.Secp256k1Wrapped
  ( CompactRecSig(..)
  , PubKey
  , SecKey
  , ToBytes32
  , derivePubKey
  , exportPubKey
  , recover
  , secKey
  , sign
  ) where

import Data.ByteString (ByteString)
import Control.DeepSeq

import qualified Crypto.Secp256k1 as Secp256k1
import Crypto.Secp256k1 (PubKey, SecKey, CompactRecSig, secKey)
import Data.String.Conv

import UnliftIO

import Data.FixedBytes

import System.Entropy


type ToBytes32 s = StringConv s Bytes32
toBytes32 :: ToBytes32 s => s -> Bytes32
toBytes32 = toS


sign :: (MonadUnliftIO m, ToBytes32 s) => SecKey -> s -> m CompactRecSig
sign sk b = do
  safeWrap $
    Secp256k1.exportCompactRecSig $ Secp256k1.signRecMsg sk m
  where m = maybe (error "Could not get sig from message") id $ Secp256k1.msg $ unFixed $ toBytes32 b

recover :: (MonadUnliftIO m, ToBytes32 s) => CompactRecSig -> s -> m (Either String PubKey)
recover crs b = do
  safeWrap do
    message <- toMessage b
    rs <- maybe (Left "Could not import sig") Right $ Secp256k1.importCompactRecSig crs
    let s = Secp256k1.convertRecSig rs
        (_, bad) = Secp256k1.normalizeSig s
    if bad
       then Left "sig not normalized"
       else do
         let r = Secp256k1.recover rs message
         maybe (Left "Could not recover pubkey") Right r

 
exportPubKey :: MonadUnliftIO m => Bool -> PubKey -> m ByteString
exportPubKey b pk = safeWrap $ Secp256k1.exportPubKey b pk

derivePubKey :: MonadUnliftIO m => SecKey -> m PubKey
derivePubKey = safeWrap . Secp256k1.derivePubKey

genSecKey :: IO SecKey
genSecKey = do
  bytes <- getEntropy 32
  case Secp256k1.secKey bytes of
       Just sk -> pure sk
       Nothing -> fail "IO error generating secret key"

toMessage :: ToBytes32 s => s -> Either String Secp256k1.Msg
toMessage = maybe (Left "Could not get sig from message") Right . Secp256k1.msg . unFixed . toBytes32

safeWrap :: (NFData a, MonadUnliftIO m) => a -> m a
safeWrap v = mask_ $ deepseq v $ pure v

