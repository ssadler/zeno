
module TestConsensusP2P where

import TestUtils
import TestNode

import Data.Serialize
import qualified Data.Set as Set
import Data.Word
import UnliftIO
import Zeno.Consensus.P2P
import Zeno.Prelude
import Zeno.Process


unit_p2p_messages :: IO ()
unit_p2p_messages = do
  withTestNodes 2 do
    [n0, n1] <- asks (map myNodeId)
    recv <- newEmptyMVar
    zoomNode 0 do
      registerCapability 10 $ putMVar recv

    zoomNode 1 do
      replicateM_ 10 do
        sendRemote n0 10 $ GetPeers
        sendRemote n0 10 $ Peers $ Set.fromList [n0, n1]

    let takeDecoded = decodeLazy . remoteMessage <$> takeMVar recv

    replicateM_ 10 do
      Right GetPeers <- takeDecoded
      peers <- takeDecoded
      peers @?= Right (Peers $ Set.fromList [n0, n1])


zoomNode :: Int -> Zeno Node () -> Zeno [Node] ()
zoomNode i act = do
  n <- asks (!!i)
  withContext (const n) act
