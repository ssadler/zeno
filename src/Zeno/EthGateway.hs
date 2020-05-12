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



data NotarisationOnEth = NOE
  { foreignHeight :: Word32
  , foreignHash :: Sha3
  , ethHeight :: Word32
  , extraData :: ByteString
  } deriving (Show)
  
instance GetABI NotarisationOnEth where
  getABI = NOE <$> getABI <*> getABI <*> getABI <*> getABI







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
   in ( f . sigR <$> sigs
      , f . sigS <$> sigs
      , BS.pack $ getV <$> sigs
      )
  where
  getV = (+27) . sigV


ethMsg :: ByteString -> Msg
ethMsg a = maybe (error "should never happen") id $
  msg $ sha3' $ "\x19\&Ethereum Signed Message:\n32" <> sha3' a
