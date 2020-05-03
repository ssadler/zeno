{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc where

import           Data.Conduit hiding (connect)
import           Data.Conduit.JSON.NewlineDelimited
import           Data.Conduit.Network

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

data Endpoint = HttpEndpoint Request | IpcEndpoint FilePath

instance Show Endpoint where
  show (IpcEndpoint path) = show path
  show (HttpEndpoint req) = show $ getUri req


runJsonRpc :: FromJSON a => Text -> Value -> (Value -> Zeno r Value) -> Zeno r a
runJsonRpc method params act = do
  let body = "{jsonrpc,method,params,id}" .% (String "2.0", method, params, Null)
      interpretResult (v::Value) =
        case v .? "." of
             Nothing -> throw $ RPCUnexpected $ asString v
             Just r -> pure r
      interpret v = case (v .? "{error}", v .? "{result}") of
                      (Nothing, Just r) -> interpretResult r
                      (Just e, _)       -> throw $ RPCException $ asString (Object (e::Object))
  act body >>= interpret

queryHttp :: Request -> Value -> Zeno r Value
queryHttp req body = do
  let reqWithBody = setRequestBodyJSON body $ setRequestMethod "POST" req
  response <- httpJSONEither reqWithBody
  case getResponseBody response of
       Left e -> throw $ RPCException (show e)
       Right out -> pure out

queryIpc :: FilePath -> Value -> Zeno r Value
queryIpc endpoint body = do
  out <- liftIO $ do
    sock <- socket AF_UNIX Stream 0
    connect sock $ SockAddrUnix endpoint
    let mResponse = maybe (Left "No response") id <$> await
        conduit = do
          yield body .| serializer .| sinkSocket sock
          sourceSocket sock .| eitherParser .| mResponse
    runConduit conduit <* close sock
  either error pure out

queryJsonRpc :: (FromJSON a, ToJSON p) => Endpoint -> Text -> p -> Zeno r a
queryJsonRpc endpoint method params =
  traceE ("Json RPC: " ++ show (endpoint, method, asString $ toJSON params)) $ do
    let transport = case endpoint of HttpEndpoint req -> queryHttp req
                                     IpcEndpoint file -> queryIpc file
    runJsonRpc method (toJSON params) transport
