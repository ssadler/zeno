
module TestConsensusReceiveCache where

import qualified Data.IntMap.Strict as IntMap
import TestUtils

import Zeno.Consensus.Runner

rc1 = IntMap.fromList $ (\i -> (i, i)) <$> [0,50..200]

test_consensusReceiveCache :: TestTree
test_consensusReceiveCache = testGroup "consensusReceiveCache"
  [
    testCase "put item in empty cache" do
      receiveCachePut () mempty @?= IntMap.singleton 0 ()

  , testCase "put item in non empty cache" do
      receiveCachePut 201 rc1 @?= IntMap.insert 201 201 rc1

  , testCase "cache get no hits does not modify" do
      receiveCacheTake (const False) rc1 @?= ([], rc1)

  , testCase "cache get with hits deletes older" do
      receiveCacheTake (==150) rc1 @?= ([150], IntMap.fromList [(200, 200)])
      
  ]
