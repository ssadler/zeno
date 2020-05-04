{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Zeno.EthGateway where

import qualified Data.ByteString as BS

import           Control.Exception.Safe

import           Network.Ethereum.Transaction
import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Ethereum.Types

import           Zeno.Monad
import           Zeno.Prelude
import           Zeno.Data.Aeson


gatewayGetConfig :: (GetABI a, Has GethConfig r)
                 => Address -> ByteString -> Zeno r a
gatewayGetConfig gateway key = do
  ethCallABI gateway "getConfig(string)" key

gatewayGetConfigJson :: (FromJSON a, Has GethConfig r)
                     => Address -> ByteString -> Zeno r a
gatewayGetConfigJson gateway key = unJsonInABI <$> gatewayGetConfig gateway key

gatewayGetMembers :: Has GethConfig r => Address -> Zeno r (Int, [Address])
gatewayGetMembers gateway = do
  ethCallABI gateway "getMembers()" ()


type ProxyParams = (Address, Integer, ByteString)

ethMakeProxySigMessage :: ProxyParams -> Msg
ethMakeProxySigMessage (dest, nonce, callData) =
  ethMsg $
    fromAddress dest <> abi "" nonce <> callData


ethMakeProxyCallData :: ProxyParams -> [CompactRecSig] -> ByteString
ethMakeProxyCallData (dest, proxyNonce, proxyCallData) sigs =
  let (r, s, v) = exportMultisigABI sigs
   in abi proxySig (dest, proxyNonce, proxyCallData, r, s, v)
  where
  proxySig = "proxy(address,uint256,bytes,bytes32[],bytes32[],bytes)"


exportMultisigABI :: [CompactRecSig] -> ([Bytes 32], [Bytes 32], ByteString)
exportMultisigABI sigs =
  let f = bytes . fromShort
   in ( f . getCompactRecSigR <$> sigs
      , f . getCompactRecSigS <$> sigs
      , BS.pack $ getV <$> sigs
      )
  where
  getV = (+27) . getCompactRecSigV


ethMakeTransaction :: (Has GethConfig r, Has EthIdent r)
                   => Address -> ByteString -> Zeno r Transaction
ethMakeTransaction dest callData = do
  EthIdent sk myAddress <- asks has
  tx <- ethMakeTransactionWithSender myAddress dest callData
  pure $ signTx tx sk


ethMakeTransactionWithSender :: Has GethConfig r => Address -> Address -> ByteString -> Zeno r Transaction
ethMakeTransactionWithSender from to callData = do
  chainID <- pure 1 -- asks $ getChainId . has
  U256 nonce <- queryEthereum "eth_getTransactionCount" [toJSON from, "latest"]
  U256 gas <- queryEthereum "eth_estimateGas" ["{to,data,from}" .% (to, Hex callData, from)]
  U256 gasPriceRec <- queryEthereum "eth_gasPrice" ()
  let gasPrice = gasPriceRec + quot gasPriceRec 2
  pure $ Tx nonce 0 (Just to) Nothing gasPrice gas callData chainID


ethMsg :: ByteString -> Msg
ethMsg a = maybe (error "should never happen") id $
  msg $ sha3' $ "\x19\&Ethereum Signed Message:\n32" <> sha3' a
