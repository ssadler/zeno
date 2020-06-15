
module TestSapling where

import Data.Serialize

import Network.Bitcoin (parseWifH)
import Network.ZCash.Sapling
import Network.Komodo
import qualified Haskoin as H
import Haskoin.Test
import Zeno.Data.Hex

import TestUtils


arbitrarySaplingTransaction :: Gen SaplingTx
arbitrarySaplingTransaction = do
  txIn <- (:[]) <$> arbitraryTxIn komodo
  txOut <- (:[]) <$> arbitraryTxOut komodo
  let txVersion = 4
  let txLockTime = 0
  pure $ SaplingTx{..}


spec_sapling_encode :: Spec
spec_sapling_encode = do
  describe "encoding a SaplingTx" do
    it "is isomorphic" $
      property $ forAll arbitrarySaplingTransaction txIso


txIso :: SaplingTx -> Bool
txIso tx = decode (encode tx) == Right tx


-- pycc

spec_sapling_sign :: Spec
spec_sapling_sign = do
  describe "test_known_good" do
    it "simple" do
      -- tx = Tx(inputs=[TxIn(("d21633ba23f70118185227be58a63527675641ad37967e2aa461559f577aec43", 0), ScriptSig.from_address('RWqrNbM3gUr4A9kX9sMXTRyRbviLsSbjAs'), input_amount=1)], outputs=[])
      -- tx.sign(['UuKZRxAR4KYVSVKxgL8oKuyBku7bVhqbGk9onuaEzkXdaxgytpLB'])
      let op = H.OutPoint "d21633ba23f70118185227be58a63527675641ad37967e2aa461559f577aec43" 0
      let tx = nullTx { txIn = [H.TxIn op "" 0xffffffff] }
      let Right key = parseWifH komodo "UuKZRxAR4KYVSVKxgL8oKuyBku7bVhqbGk9onuaEzkXdaxgytpLB"
      let sigin = toSigIn (getAddrHash "RWqrNbM3gUr4A9kX9sMXTRyRbviLsSbjAs") op 1
      let Right signed = signTxSapling komodo tx [sigin] [key]
      let Right decoded = decode $ unHex "0400008085202f890143ec7a579f5561a42a7e9637ad4156672735a658be2752181801f723ba3316d2000000006a4730440220502507cbbdaa870ca7d75a1b0ccb370d9ebd4e13137e926891637655a027c0ef02201bda31cb065593fb9cbf5ee06b20bcdb0a73936371e8caf69717e396c4e938ca0121038c3d482cd29f75ce997737705fb5287f022ded66093ee7d929aea100c5ef8a63ffffffff0000000000000000000000000000000000000000"
      decoded { txIn = [] } @?= signed { txIn = [] }
      txIn decoded @?= txIn signed :: IO ()
      --toHex (encode signed) @?= 


toSigIn a op val = H.SigInput (H.PayPKHash a) val op H.sigHashAll Nothing
