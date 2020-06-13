
module TestNotarise where

import Data.Word
import Zeno.Notariser.Step

import TestUtils


-- test_minNextHeight = testGroup "minNextHeight"
--   [ testProperty "always larger than last height" do
--       x <- choose (1, 100)
--       h <- arbitrary
--       pure $ getMinNextHeight x h > h
-- 
--   , testProperty "stable" do
--       x <- choose (1, 100)
--       h <- arbitrary
--       pure $ mod (getMinNextHeight x h) x == 0
-- 
--   , testProperty "within interval" do
--       x <- choose (1, 100)
--       h <- arbitrary
--       pure $ mod (getMinNextHeight x h) x < x + h
--   ]

-- What do we do if the last notarised block is higher than the current one?
-- Clearly this could happen if a node is out of sync. We should wait for
-- the current one in this case. But, in the case that we wait, we should
-- not try to notarise but start the step again.

test_getNextHeight = testGroup "getNextHeight"
  [
    testCase "fixed examples" do
             -- interval last current
      getNextHeight 10  0  0 @?= 10
      getNextHeight 10 10 10 @?= 20
      getNextHeight 10 20 50 @?= 50
      getNextHeight 10 20 49 @?= 40

  , testProperty "always higher than last height" do
      interval <- choose (1, 100)
      current <- choose (0, 2^31)
      last <- choose (0, 2^31)
      let r = getNextHeight interval last current
      pure $ counterexample (show (interval, current, last, r)) $ r > last

  , testProperty "stable" do
      interval <- choose (1, 100)
      current <- choose (0, 2^31)
      last <- choose (0, 2^31)
      pure $ mod (getNextHeight interval last current) interval == 0

  , testProperty "doesnt wrap" do
      interval <- choose (1, 100)
      current <- choose (0, 2^31)
      last <- choose (0, 2^31)
      let next = getNextHeight interval last current
      pure $ counterexample (show (interval, current, last, next)) $ next > (min current last - interval)

  ]
