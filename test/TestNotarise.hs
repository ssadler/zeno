
module TestNotarise where

import Zeno.Notariser.KMDDpow

import TestUtils


test_notariser = testGroup "Notariser"
  [ testCase "getNextHeight" do
      getNextHeight 10  0  0 @?= 10
      getNextHeight 10 10 10 @?= 20
      getNextHeight 10 20 50 @?= 50
      getNextHeight 10 20 49 @?= 40
  ]
