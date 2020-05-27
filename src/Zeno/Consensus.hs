
module Zeno.Consensus
  ( module Zeno.Consensus.Types
  , startSeedNode
  , withConsensusNode
  , runConsensus
  , propose
  , step
  , stepWithTopic
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
import Zeno.Console


-- Node -----------------------------------------------------------------------

withConsensusNode :: ConsensusNetworkConfig -> (ConsensusNode -> Zeno r a) -> Zeno r a
withConsensusNode CNC{..} act = do
  (_, node) <- allocate (getTransport >>= startNode) stopNode
  p2p <- withContext (\_ -> node) $ startP2P seeds'
  act $ ConsensusNode node p2p
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
    createTransport tcpHost tcpParams <&>
      either (error . show) id
  
  -- TODO: get someone knowledgeable about networking to validate
  -- https://hackage.haskell.org/package/network-transport-tcp-0.7.0/docs/src/Network.Transport.TCP.html#defaultTCPParameters
  tcpParams = defaultTCPParameters
    { tcpCheckPeerHost = True
    , tcpMaxReceiveLength = 10000 -- 64 * 128 + margin
    , tcpMaxAddressLength = 128
    }

  -- | Make a NodeId from "host:port" string.
  makeNodeId :: Word16 -> String -> NodeId
  makeNodeId port addr = NodeId . EndPointAddress $ BS8.pack addr' <> ":0"
    where addr' = addr ++ maybe (':' : show port) (\_ -> "") (elemIndex ':' addr)


startSeedNode :: String -> Word16 -> IO ()
startSeedNode host port = do
  let cnc = CNC [] host port
  withConsoleUI $ \ui -> do
    runZeno ui () $ withConsensusNode cnc \_ -> threadDelay $ 2^62
