
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , startSeedNode
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
import Zeno.Process

import Zeno.Consensus.Types
import Zeno.Consensus.Round
import Zeno.Consensus.P2P

import Zeno.Config
import Zeno.Prelude


-- Node -----------------------------------------------------------------------

withConsensusNode :: ConsensusNetworkConfig -> (ConsensusNode -> IO a) -> IO a
withConsensusNode CNC{..} act = do
  runZenoR () do
    (_, node) <- allocate (getTransport >>= startNode) stopNode
    zenoReader (\_ -> node) do
      p2p <- startP2P seeds'
      liftIO $ act $ ConsensusNode node p2p
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


startSeedNode :: String -> Word16 -> IO ()
startSeedNode host port = do
  let cnc = CNC [] host port
  withConsensusNode cnc \_ -> threadDelay $ 2^62
