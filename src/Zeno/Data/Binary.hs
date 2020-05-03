{-# LANGUAGE FlexibleInstances #-}

module Zeno.Data.Binary
  ( module ALL
  , Ser2Bin(..)
  ) where

import           Data.Binary as ALL
import qualified Data.Serialize as Ser
import           Data.Typeable


newtype Ser2Bin a = Ser2Bin { unSer2Bin :: a }
  deriving (Typeable, Show)

instance Ser.Serialize a => Binary (Ser2Bin a) where
  put = put . Ser.encode . unSer2Bin
  get = do
    bs <- get
    either fail (pure . Ser2Bin) $ Ser.decode bs
