
module Zeno.Process.Node.ReceiveMissCache where


import qualified Data.IntMap.Strict as IntMap
import Zeno.Process.Types


receiveCachePut :: ReceiveMiss -> ReceiveMissCache -> ReceiveMissCache
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


receiveCacheTake :: ProcessId -> ReceiveMissCache -> ([ReceiveMiss], ReceiveMissCache)
receiveCacheTake pid cache =
  let (matches, nextCache) = IntMap.partition (\(pid', _, _) -> pid == pid') cache
   in (IntMap.elems matches, nextCache)
