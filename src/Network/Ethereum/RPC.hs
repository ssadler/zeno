{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.RPC where

import           Control.Concurrent (threadDelay)
import           Control.Exception.Safe

import           Network.Ethereum.Data
import           Network.Ethereum.Crypto
import           Network.Ethereum.Transaction as Tx
import           Network.Ethereum.Types

import           Network.JsonRpc

import           Zeno.Data.Aeson
import           Zeno.Monad
import           Zeno.Prelude


queryEthereum :: (Has GethConfig r, ToJSON b, FromJSON a) => Text -> b -> Zeno r a
queryEthereum method params = do
  uri <- asks $ gethEndpoint . has
  let endpoint = HttpEndpoint $ fromString uri
  queryJsonRpc endpoint method params


readCall :: (Has GethConfig r, FromJSON a) => Address -> ByteString -> Zeno r a
readCall addr callData =
  queryEthereum "eth_call" ["{to,data}" .% (addr, Hex callData), "latest"]

ethCallABI :: (Has GethConfig r, GetABI a, PutABI p, Show p) => Address -> String -> p -> Zeno r a
ethCallABI addr sig params = do
  onException 
    (unABI <$> readCall addr (abi sig params))
    (logError $ printf "Error in eth_call: %s with %s" sig (show params))

postTransactionSync :: Has GethConfig r => Transaction -> Zeno r Value
postTransactionSync tx = do
  -- logInfo $ "Testing transaction"
  -- callResult <- readCall (fromJust $ _to tx) (Tx._data tx)
  -- logInfo $ "Result: " ++ asString (callResult :: Value)
  logInfo $ "Sending transaction: " ++ (show $ txid tx)
  txid <- queryEthereum "eth_sendRawTransaction" [toJSON $ Hex $ encodeTx $ tx]
  logInfo $ "Send transaction, txid: " <> show txid
  fix $
    \wait -> do
      liftIO $ threadDelay 1000000
      queryEthereum "eth_getTransactionReceipt" [txid::Value] >>=
        \case
          Null -> wait
          o | o .? "{status}" == Just (U256 1) -> pure o
          o -> error $ "Unknown transaction status: " ++ show o

data RPCMaybe a = RPCMaybe (Maybe a)
  deriving (Show)

instance FromJSON a => FromJSON (RPCMaybe a) where
  parseJSON (String "0x") = pure $ RPCMaybe Nothing
  parseJSON val = RPCMaybe . Just <$> parseJSON val


eth_getTransactionReceipt :: Has GethConfig r => Sha3 -> Zeno r TransactionReceipt
eth_getTransactionReceipt h = queryEthereum "eth_getTransactionReceipt" [h]

eth_blockNumber :: Has GethConfig r => Zeno r U256
eth_blockNumber = queryEthereum "eth_blockNumber" ()

eth_getBlockByHash :: Has GethConfig r => Sha3 -> Zeno r EthBlock
eth_getBlockByHash n = queryEthereum "eth_getBlockByHash" (n, False)

eth_getBlockByNumber :: Has GethConfig r => U256 -> Zeno r EthBlock
eth_getBlockByNumber n = queryEthereum "eth_getBlockByNumber" (n, False)
