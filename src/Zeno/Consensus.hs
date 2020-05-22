
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , startSeedNode
  , spawnConsensusNode
  , withConsensusNode
  , runConsensus
  , propose
  , step
  , stepWithTopic
  , collectGeneric
  , collectMajority
  , collectThreshold
  , collectMembers
  , majorityThreshold
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.Transport
import Network.Transport.TCP
import Network.Distributed

import Zeno.Consensus.Types
import Zeno.Consensus.Round
import Zeno.Consensus.P2P

import Zeno.Config
import Zeno.Prelude


-- Node -----------------------------------------------------------------------

spawnConsensusNode :: ConsensusNetworkConfig -> IO ConsensusNode
spawnConsensusNode CNC{..} = do
  t <- getTransport
  node <- startNode t
  p2p <- startP2P node seeds'
  pure $ ConsensusNode node p2p

  where
  myNodeId = makeNodeId port host
  seeds' = makeNodeId port <$> seeds

  (bindAddr, hostAddr) =
    case elemIndex '/' host of
      Nothing -> (host, host)
      Just idx -> let (a, _:b) = splitAt idx host in (a, b)

  tcpHost = Addressable $
    TCPAddrInfo bindAddr (show port) ((,) hostAddr)

  getTransport :: IO Transport
  getTransport = do
    let tcpParams = defaultTCPParameters { tcpCheckPeerHost = True }
    createTransport tcpHost tcpParams <&>
      either (error . show) id


  -- | Make a NodeId from "host:port" string.
  makeNodeId :: Word16 -> String -> NodeId
  makeNodeId port addr = NodeId . EndPointAddress $ BS8.pack addr' <> ":0"
    where addr' = addr ++ maybe (':' : show port) (\_ -> "") (elemIndex ':' addr)


withConsensusNode :: ConsensusNetworkConfig -> (ConsensusNode -> IO a) -> IO a
withConsensusNode conf = do
 bracket (spawnConsensusNode conf)
         (closeNode . node)


startSeedNode :: String -> Word16 -> IO ()
startSeedNode host port = do
  let cnc = CNC [] host port
  _ <- spawnConsensusNode cnc
  threadDelay $ 2^62
