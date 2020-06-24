
module Zeno.Process.Node.ReceiveMissCache where

import Lens.Micro.Platform
import Data.IntMap.Strict as IntMap


receiveCachePut :: a -> IntMap a -> IntMap a
receiveCachePut miss cache
  | length cache == 0 = IntMap.singleton 0 miss
  | length cache == cacheMaxSize =
      let ((minId, _), nextCache) = IntMap.deleteFindMin cache
       in IntMap.insert (minId + length cache) miss nextCache
  | otherwise =
      let (minId, _) = IntMap.findMin cache
       in IntMap.insert (minId + length cache) miss cache
  where
  cacheMaxSize = 1000


receiveCacheTake :: (a -> Bool) -> IntMap a -> ([a], IntMap a)
receiveCacheTake f = over _1 IntMap.elems . IntMap.partition f
