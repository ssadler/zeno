
module TestNotariserShuffle where

import TestUtils hiding ((.&.))

import Data.Bits
import Data.List
import Data.Word
import Zeno.Notariser.Shuffle


test_notariserShuffle :: TestTree
test_notariserShuffle = testGroup "notariserShuffle"
  [
    testProperty "output always isomorphic to input"
      \i word -> do
        let inp = [0..mod i 100 :: Int]
            r = sort $ shuffleWithWords inp [word..]
         in inp == sort r

  , testProperty "when the length is a power of two, never need to skip" do
      i <- choose (1, 8::Int)
      word <- arbitrary
      pure $ (==1) $ length $ take 1 $ shuffleWithWords (replicate (2^i) ()) [word]

  , testProperty "when length is power of 2, can always choose with one word" do
      n <- (2^) <$> choose (1, 8::Int)
      i <- choose (0, n-1)
      pure $ (==[i+1]) $ take 1 $ shuffleWithWords [1..n] [i]

  , testProperty "unneeded bits are not used" do
      n <- (2^) <$> choose (1, 8::Int)
      i <- choose (0, n-1)
      pure $ (==[i+1]) $ take 1 $ shuffleWithWords [1..n] [i + shift i 16]

  , testProperty "shuffle identity"
      \inp -> do
        inp == shuffleWithWords (inp::[Int]) (repeat 0)

  , testProperty "skips numbers that are not in range"
      \i' ->
        let i = mod i' 1000 + 3
            s = 0 == i .&. (i-1)
         in
           counterexample (show $ (i', i, s)) $
             head (shuffleWithWords [1..i::Int] [fromIntegral i, 2]) == if s then 1 else 3

  , testCase "skips numbers that are not in range" do
      head (shuffleWithWords [1..1] [1, 2]) @?= 1
      head (shuffleWithWords [1..2] [2, 2]) @?= 1
      head (shuffleWithWords [1..3] [3, 2]) @?= 3
      head (shuffleWithWords [1..4] [4, 2]) @?= 1
      head (shuffleWithWords [1..5] [5, 2]) @?= 3
      head (shuffleWithWords [1..6] [6, 2]) @?= 3
      head (shuffleWithWords [1..7] [7, 2]) @?= 3
      head (shuffleWithWords [1..8] [8, 2]) @?= 1
      head (shuffleWithWords [1..9] [9, 2]) @?= 3

  ]
