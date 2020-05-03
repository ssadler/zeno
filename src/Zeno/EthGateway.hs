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


newtype EthGateway = EthGateway { unEthGateway :: Address }
  deriving (Show, PutABI)

gatewayGetConfig :: (GetABI a, Has GethConfig r, Has EthGateway r)
                 => ByteString -> Zeno r a
gatewayGetConfig key = do
  addr <- unEthGateway . has <$> ask
  ethCallABI addr "getConfig(string)" key

gatewayGetConfigJson :: (FromJSON a, Has GethConfig r, Has EthGateway r)
                     => ByteString -> Zeno r a
gatewayGetConfigJson key = unJsonInABI <$> gatewayGetConfig key

gatewayGetMembers :: (Has GethConfig r, Has EthGateway r) => Zeno r (Int, [Address])
gatewayGetMembers = do
  addr <- unEthGateway . has <$> ask
  ethCallABI addr "getMembers()" ()


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
  getV = encodeSpecialV (ChainId 1) . getCompactRecSigV


ethMakeTransaction :: (Has GethConfig r, Has EthIdent r)
                   => Address -> ByteString -> Zeno r Transaction
ethMakeTransaction dest callData = do
  EthIdent sk myAddress <- asks has
  chainID <- pure 1 -- asks $ getChainId . has
  U256 nonce <- queryEthereum "eth_getTransactionCount" [toJSON myAddress, "latest"]
  U256 gas <- queryEthereum "eth_estimateGas" ["{to,data,from}" .% (dest, Hex callData, myAddress)]
  U256 gasPriceRec <- queryEthereum "eth_gasPrice" ()
  let gasPrice = gasPriceRec + quot gasPriceRec 2
  let tx = Tx nonce 0 (Just dest) Nothing gasPrice gas callData chainID
  pure $ signTx tx sk



ethMsg :: ByteString -> Msg
ethMsg a = maybe (error "should never happen") id $
  msg $ sha3' $ "\x19\&Ethereum Signed Message:\n32" <> sha3' a
