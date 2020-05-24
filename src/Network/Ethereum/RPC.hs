{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Ethereum.RPC where

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


queryAccountNonce :: Has GethConfig r => Address -> Zeno r Integer
queryAccountNonce addr =
  unU256 <$> queryEthereum "eth_getTransactionCount" [toJSON addr, "latest"]


postTransaction :: Has GethConfig r => Transaction -> Zeno r Sha3
postTransaction tx = do
  logInfo $ "Sending transaction: " ++ (show $ hashTx tx)
  queryEthereum "eth_sendRawTransaction" [toJSON $ Hex $ encodeTx $ tx]


-- Waits for a transaction to be confirmed with 1 extra block
-- There's a detail here - we should probably wait for it to be confirmed with one
-- or two extra blocks, but, testing is being done with ganache which mines blocks on demand.
-- TODO.
waitTransactionConfirmed1 :: Has GethConfig r => Int -> Sha3 -> Zeno r (Maybe Value)
waitTransactionConfirmed1 timeout txid = do
  let delay = min timeout 5000000
  r <- queryEthereum "eth_getTransactionReceipt" [txid]
  pure r >>=
    \case
      o | isJust (o .? "{blockNumber}" :: Maybe U256) -> pure $ Just o
      _ | timeout == 0 -> pure Nothing
      _ -> liftIO (threadDelay delay) >> waitTransactionConfirmed1 (timeout - delay) txid


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
