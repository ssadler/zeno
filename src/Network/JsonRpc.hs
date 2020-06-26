{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc
  ( JsonRPCArgs(..)
  , RPCException(..)
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


class ToJSON a => JsonRPCArgs a
instance ToJSON a => JsonRPCArgs [a]
instance JsonRPCArgs ()
instance (ToJSON a, ToJSON b) => JsonRPCArgs (a, b)
instance (ToJSON a, ToJSON b, ToJSON c) => JsonRPCArgs (a, b, c)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => JsonRPCArgs (a, b, c, d)


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

queryJsonRpc :: (FromJSON a, JsonRPCArgs p) => Endpoint -> Text -> p -> Zeno r a
queryJsonRpc endpoint method params = do
  let
    transport = case endpoint of HttpEndpoint req -> queryHttpJson req
    body = createRequest method params
    go = do
      res <- transport body
      case res .? "{error}" of
        Just e | e /= Null ->
          throwIO $ RPCError e
        _ ->
          case res .? "{result}" of
            Just r -> pure r
            Nothing -> throwIO $ RPCUnexpectedResult res

  onException go $ logWarnNS "rpc" $ "Error during request: %s %s" % (show endpoint, jsonString body)


jsonString :: Value -> String
jsonString = toS . encode
