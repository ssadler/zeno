{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zeno.Data.Hex
  ( Hex(..)
  ) where

import qualified Data.Text as T

import           Data.Char
import           Zeno.Data.Aeson
import           Zeno.Prelude


newtype Hex = Hex { unHex :: ByteString }

instance Show Hex where
  show (Hex bs) = show $ "0x" <> toHex bs

instance Read Hex where
  readsPrec a s
    | take 2 s == "0x" = readsPrec a $ drop 2 s
    | otherwise        = [(Hex $ fromHex $ fromString s, "")]

instance ToJSON Hex where
  toJSON (Hex bs) = String $ decodeUtf8 $ "0x" <> toHex bs
  {-# INLINABLE toJSON #-}

instance FromJSON Hex where
  parseJSON val = do
    s <- parseJSON val
    let r = if T.take 2 s == "0x" then T.drop 2 s else s
     in Hex <$> fromJsonHex (String r)
  {-# INLINABLE parseJSON #-}
