
module Zeno.Notariser.Common.KMD where

import qualified Data.Map as Map
import Data.Serialize
import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling
import qualified Haskoin as H

import Network.Ethereum (Address, deriveEthAddress)
import Zeno.Consensus
import Zeno.Console
import Zeno.Notariser.Types
import Zeno.Prelude
import Zeno.Notariser.UTXO


runKmdThreads :: Zeno EthNotariser ()
runKmdThreads = do
  forkMonitorUTXOs 5 20

scriptVout :: Word64 -> H.ScriptOutput -> H.TxOut
scriptVout value = H.TxOut value . H.encodeOutputBS

nullDataVout :: ByteString -> H.TxOut
nullDataVout = scriptVout 0 . H.DataCarrier

kmdDataOutputs :: Word64 -> H.ScriptOutput -> ByteString -> [H.TxOut]
kmdDataOutputs amount recip opret = 
   [ H.TxOut amount $ H.encodeOutputBS recip
   , H.TxOut 0 $ H.encodeOutputBS $ H.DataCarrier opret
   ]
