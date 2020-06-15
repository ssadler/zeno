{-# LANGUAGE OverloadedStrings #-}

module Spec where

import           Crypto.Secp256k1.Recoverable
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as BS

import           Network.Ethereum.Crypto
import           Network.Ethereum.Transaction
import           Zeno.Prelude

import           Test.Tasty
import           Test.Tasty.HUnit


test_recSigTests :: TestTree
test_recSigTests = testGroup "RecSig tests"
  [ 
     testCase "RecSig" $ do
      sig <- signIO sk txid_a
      toRSV sig @?= txsig_a

  , testCase "Get txid" $ do
      hashTx tx_a @?= txid_a

  , testCase "Encode tx" $ do
      encodeTx tx_a @?= txbin_a

  , testCase "RecoverFrom" $ do
      signed <- signTx sk tx_a
      let Just sig = _sig signed
      let m = hashTx $ signed { _sig = Nothing }
      recoverAddr m sig >>= (@?= address)

  , testCase "Recover" $ do
      let txid = hashTx tx_a
      sig <- signIO sk txid
      a <- recoverAddr txid sig
      a @?= address

  , testCase "encodeSpecialV" $ do

      encodeSpecialV 16 0 @?= 67

      forM_ [0..123::ChainId] $ \i -> do
        decodeSpecialV (encodeSpecialV i 0) @?= (i, 0)
        decodeSpecialV (encodeSpecialV i 1) @?= (i, 1)
  ]


sk :: SecKey
sk = "3131313131313131313131313131313131313131313131313131313131313131"

address :: Address
EthIdent _ address = unsafePerformIO $ deriveEthIdent sk

txid_a :: PrefixedHex 32
txid_a = "d018f1502a71f61a00b77546b99f2a647dda07ecb4cf94bd14cd4dbf4337be3d"

txbin_a :: ByteString
txbin_a = fst $ B16.decode "ce800a82cfc6808204d281f4108080"

tx_a :: Transaction
tx_a = Tx { _nonce = 0
          , _value = 1234
          , _to = Nothing
          , _sig = Nothing
          , _gasPrice = 10
          , _gas = 53190
          , _data = "\xf4"
          , _chainId = 16
          }

txsig_a :: (Bytes32, Bytes32, Word8)
txsig_a = ( "14f469b1b8022b411cbe6e6b19b21578223bb81be0f32048bc258baa905a47d0"
          , "37a2c7cc63e8f8b3523700ad23709e2ae7582e62b78b7e6e178c203b5a863e68"
          , 1
          )
