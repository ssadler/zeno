{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc
  ( RPCException(..)
  , RPCTransportException(..)
  , Endpoint(..)
  , queryJsonRpc
  ) where

import qualified Data.ByteString as BS
import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.Socket hiding (send, recv)

import           Data.Aeson (encode)
import           Zeno.Data.Aeson
import           Zeno.Monad
import           Zeno.Prelude


data RPCException
  = RPCError Value
  | RPCUnexpectedResult Value

instance Show RPCException where
  show (RPCError v) = "RPCError: " ++ jsonString v
  show (RPCUnexpectedResult v) = "RPCUnexpected: " ++ jsonString v

instance Exception RPCException

data RPCTransportException
  = RPCJsonException JSONException
  | RPCHttpException HttpException
  deriving (Show)

instance Exception RPCTransportException

data Endpoint = HttpEndpoint Request

instance Show Endpoint where
  show (HttpEndpoint req) = show $ getUri req


createRequest :: ToJSON a => Text -> a -> Value
createRequest method params =
  object [ "jsonrpc" .= String "2.0"
         , "method"  .= method
         , "params"  .= params
         , "id"      .= Number 1
         ]

queryHttpJson :: Request -> Value -> Zeno r Value
queryHttpJson req body = do
  let reqWithBody = setRequestBodyJSON body $ setRequestMethod "POST" req
  catch
    do
      response <- httpJSONEither reqWithBody
      case getResponseBody response of
           Left e -> throwIO $ RPCJsonException e
           Right out -> pure out

    \e -> throwIO $ RPCHttpException e

queryJsonRpc :: (FromJSON a, ToJSON p) => Endpoint -> Text -> p -> Zeno r a
queryJsonRpc endpoint method params = do
  let transport = case endpoint of HttpEndpoint req -> queryHttpJson req
      body = createRequest method params
  traceE ("Json RPC: " ++ show (endpoint, jsonString body)) do
    res <- transport body
    case res .? "{error}" of
      Just e | e /= Null ->
        throwIO $ RPCError e
      _ ->
        case res .? "{result}" of
          Just r -> pure r
          Nothing -> throwIO $ RPCUnexpectedResult res


jsonString :: Value -> String
jsonString = toS . encode
