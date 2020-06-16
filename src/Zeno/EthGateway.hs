
module Zeno.EthGateway where

import qualified Data.ByteString as BS

import           Crypto.Secp256k1.Recoverable
import           Control.Exception.Safe

import           Network.Ethereum.Transaction
import           Network.Ethereum.Crypto
import           Network.Ethereum.Data
import           Network.Ethereum.RPC
import           Network.Ethereum.Types

import           Zeno.Monad
import           Zeno.Prelude
import           Zeno.Data.Aeson



data EthNotarisationData = NOE
  { noeForeignHeight :: Word32
  , noeForeignHash :: Bytes32
  , noeLocalHeight :: Word32
  , noeExtraData :: ByteString
  } deriving (Eq, Show)
  
instance GetABI EthNotarisationData where
  getABI = NOE <$> getABI <*> getABI <*> getABI <*> getABI



gatewayGetConfig :: (GetABI a, Has GethConfig r)
                 => Address -> ByteString -> Zeno r a
gatewayGetConfig gateway key = do
  ethCallABI gateway "getConfig(string)" key

gatewayGetConfigJson :: (FromJSON a, Has GethConfig r)
                     => Address -> ByteString -> Zeno r a
gatewayGetConfigJson gateway key = unJsonInABI <$> gatewayGetConfig gateway key

gatewayGetMembers :: Has GethConfig r => Address -> Zeno r (Int, [Address])
gatewayGetMembers gateway = ethCallABI gateway "getMembers()" ()


type ProxyParams = (Address, Integer, ByteString)

ethMakeProxySigMessage :: ProxyParams -> Bytes32
ethMakeProxySigMessage (dest, nonce, callData) =
  ethMsg $ toS dest <> abi "" nonce <> callData


ethMakeProxyCallData :: ProxyParams -> [RecSig] -> ByteString
ethMakeProxyCallData (dest, proxyNonce, proxyCallData) sigs =
  let (r, s, v) = exportMultisigABI sigs
   in abi proxySig (dest, proxyNonce, proxyCallData, r, s, v)
  where
  proxySig = "proxy(address,uint256,bytes,bytes32[],bytes32[],bytes)"

type NotarisationParams = (Word32, Bytes32, ByteString)

ethMakeNotarisationCallData :: NotarisationParams -> ByteString
ethMakeNotarisationCallData = abi "notarise(uint256,bytes32,bytes)"


exportMultisigABI :: [RecSig] -> ([Integer], [Integer], ByteString)
exportMultisigABI sigs =
  over _3 (BS.pack . fmap (+27)) $ unzip3 $ toRSV <$> sigs


ethGetLastNotarisationAndSequence :: Has GethConfig r => Address -> Zeno r (Maybe (EthNotarisationData, Int))
ethGetLastNotarisationAndSequence notarisationsContract = do
  (r@(NOE h _ _ _), sequence) <- ethCallABI notarisationsContract "getLastNotarisation()" ()
  pure if h == 0 then Nothing else Just (r, fromIntegral $ unU256 sequence)


ethMsg :: ByteString -> Bytes32
ethMsg a = sha3b $ "\x19\&Ethereum Signed Message:\n32" <> sha3' a
