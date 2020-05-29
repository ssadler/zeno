{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc
  ( RPCException(..)
  , Endpoint(..)
  , queryJsonRpc
  ) where

import           Network.HTTP.Client
import           Network.HTTP.Simple
import           Network.Socket hiding (send, recv)

import           Zeno.Data.Aeson
import           Zeno.Monad
import           Zeno.Prelude


data RPCException =
    RPCUnexpected String
  | RPCException String
  deriving (Show)

instance Exception RPCException

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

queryHttp :: Request -> Value -> Zeno r Value
queryHttp req body = do
  let reqWithBody = setRequestBodyJSON body $ setRequestMethod "POST" req
  response <- httpJSONEither reqWithBody
  case getResponseBody response of
       Left e -> throwIO $ RPCException (show e)
       Right out -> pure out

queryJsonRpc :: (FromJSON a, ToJSON p) => Endpoint -> Text -> p -> Zeno r a
queryJsonRpc endpoint method params = do
  let transport = case endpoint of HttpEndpoint req -> queryHttp req
      req = createRequest method params
  traceE ("Json RPC: " ++ show (endpoint, asString req)) do
    res <- transport req
    case res .? "{error}" of
      Just e | e /= Null ->
        throwIO $ RPCException $ asString (e::Value)
      _ ->
        case res .? "{result}" of
          Just r -> pure r
          Nothing -> throwIO $ RPCUnexpected $ asString res
