{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}


module Zeno.Prelude
  ( module ALL
  , PercentFormat(..)
  , (<&>)
  , traceE
  , fromHex
  , toHex
  , expandPath
  ) where

import Control.Applicative as ALL
import Control.Exception.Safe as ALL
import Control.Monad as ALL (forM, forM_, join, when, replicateM, foldM, forever)
import Control.Monad.IO.Class as ALL (liftIO)
import Control.Monad.Reader as ALL (ask, asks)
import Control.Monad.Trans.Class as ALL
import GHC.Generics as ALL (Generic)

import Data.Aeson as ALL (Value)
import Data.Aeson.Quick as ALL ((.?))
import Data.ByteString as ALL (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as ALL (toStrict, fromStrict)
import Data.ByteString.Short as ALL (ShortByteString, toShort, fromShort)
import Data.Either as ALL (fromRight)
import Data.Function as ALL (fix)
import Data.List as ALL (elemIndex, find, findIndex, sort, sortOn)
import Data.Map as ALL (Map)
import Data.Maybe as ALL (catMaybes, isJust, fromJust, fromMaybe, mapMaybe, listToMaybe)
import Data.Monoid as ALL
import Data.Set as ALL (Set)
import Data.String.Conv as ALL
import Data.String as ALL (IsString, fromString)
import Data.Text as ALL (Text, unpack)
import Data.Text.Encoding as ALL (encodeUtf8, decodeUtf8)
import Data.Time.Clock as ALL (UTCTime, getCurrentTime)
import Data.Word as ALL (Word8, Word16, Word32, Word64)

import UnliftIO.Concurrent as ALL (threadDelay, forkIO)

import Network.Ethereum.Errors as ALL
import Zeno.Monad as ALL
import Zeno.Logging as ALL

import Text.Printf as ALL (PrintfArg, printf)

import System.Directory

import Debug.Trace as ALL (traceShowId)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>

traceE :: String -> Zeno r a -> Zeno r a
traceE prefix act = do
  r <- ask
  let log e = do runZeno () (logError prefix) >> throw e
  liftIO $ do
    runZeno r act `catchAny` log

fromHex :: ByteString -> ByteString
fromHex bs =
  if BS.take 2 bs == "0x"
     then fromHex $ BS.drop 2 bs
     else let (b,r) = B16.decode bs
           in if r /= "" then error "Invalid hex" else b

toHex :: ByteString -> ByteString
toHex = B16.encode


expandPath :: FilePath -> IO FilePath
expandPath ('~':xs) = (++xs) <$> getHomeDirectory
expandPath p        = pure p


class PercentFormat a where
  (%) :: String -> a -> String
  default (%) :: PrintfArg a => String -> a -> String
  s % a = printf s a

instance PercentFormat String
instance PercentFormat Word32
instance PercentFormat Integer
instance PercentFormat Int


instance (PrintfArg a, PrintfArg b) => PercentFormat (a, b) where
  s % (a, b) = printf s a b

instance (PrintfArg a, PrintfArg b, PrintfArg c) => PercentFormat (a, b, c) where
  s % (a, b, c) = printf s a b c
