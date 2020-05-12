
module TestSapling where

import qualified Data.Serialize as S

import Network.ZCash.Sapling
import Haskoin.Test
import Network.Komodo

import TestUtils


arbitrarySaplingTransaction :: Gen SaplingTx
arbitrarySaplingTransaction = do
  txIn <- (:[]) <$> arbitraryTxIn komodo
  txOut <- (:[]) <$> arbitraryTxOut komodo
  let txVersion = 4
  let txLockTime = 0
  pure $ SaplingTx{..}


spec_sapling :: Spec
spec_sapling = do
  describe "encoding a SaplingTx" do
    it "is isomorphic" $
      property $ forAll arbitrarySaplingTransaction txIso


txIso :: SaplingTx -> Bool
txIso tx = S.decode (S.encode tx) == Right tx
