{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Errors
  ( Err(..)
  , ErrClass(..)
  , allErrorClasses
  , errMsg
  , errStr
  , otherErr
  ) where


import Data.Aeson
import Data.Text
import GHC.Generics


data Err = Err Value
  deriving (Eq, Generic, Show)

instance ToJSON Err where
  toJSON (Err val) = val


data ErrClass =
    InvalidJson
  | InvalidMethod
  | InvalidProtocol
  | InvalidParams
  | OtherError
  | RPCMethodError
  | RPCTransportError
  deriving (Enum, Eq, Generic, Show)

instance ToJSON ErrClass where
  toEncoding = genericToEncoding defaultOptions


allErrorClasses :: [ErrClass]
allErrorClasses = [InvalidJson ..]


errMsg :: ErrClass -> Text -> Err
errMsg code msg = Err $ object [ "class" .= code, "msg" .= msg ]


errStr :: ErrClass -> String -> Err
errStr code msg = Err $ object [ "class" .= code, "msg" .= msg ]


otherErr :: String -> Err
otherErr msg = Err $ object [ "class" .= OtherError, "msg" .= msg ]
