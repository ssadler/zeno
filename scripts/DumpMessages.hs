
module DumpMessages where

import Network.Transport
import Network.Ethereum.Crypto
import Data.Serialize
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import Data.Char
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set

import Zeno.Prelude
import Zeno.Process
import Zeno.Consensus.P2P
import Zeno.Consensus.Types


dump :: (Serialize a, Show a) => a -> IO ()
dump a = do
  print a
  BS8.putStrLn $ toHex $ encode a
  putStrLn ""

sk :: SecKey
sk = "12f72e7ca1a25292e913319d9b28e40c931db883b6bb2c29ea4834b991c7053a"

sig =  sign sk "12f72e7ca1a25292e913319d9b28e40c931db883b6bb2c29ea4834b991c7053a"

dumpMessages :: IO ()
dumpMessages = do

  let pid = peerControllerPid
  let nid i = NodeId $ EndPointAddress $ toS $ "127.0.0.1:4044" ++ show i ++ ":0:8"
  let hello = (peerControllerPid, GetPeers)
  let peers = (peerControllerPid, Peers $ Set.fromList [nid 2, nid 3, nid 4])

  print "Process ID of Peer Controller"
  dump $ peerControllerPid

  print "1: GetPeers - node sends message to p2p service on another node"
  dump hello

  print "2: node replied with peer list"
  dump peers


  let inventoryIndex = InventoryIndex 5 :: StepMessage Int
      getInventory = GetInventory 50 :: StepMessage Int
      inventoryData = InventoryData $ Map.singleton "0x90f8bf6a479f320ead074411a4b0e7944ea8c9c1" (sig, 0xDEADBEEF) :: StepMessage Int
      someRandomPid = hashServiceId ""

  print ""
  print ""

  print "1: InventoryIndex: node broadcasts what index it has (it's a bitmask of the peer IDs)"
  dump (someRandomPid, inventoryIndex)

  print "2: GetInventory: node asks for inventory"
  dump (someRandomPid, getInventory)

  print "3: InventoryData: node responds with inventory"
  dump (someRandomPid, inventoryData)
