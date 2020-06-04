
module Zeno.Notariser.Common.KMD
  ( runKmdThreads
  , kmdDataOutputs
  , kmdInputAmount
  ) where

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


kmdInputAmount :: Word64
kmdInputAmount = 9800


runKmdThreads :: Zeno EthNotariser ()
runKmdThreads = do
  forkMonitorUTXOs kmdInputAmount 5 20


kmdDataOutputs :: Word64 -> H.ScriptOutput -> ByteString -> [H.TxOut]
kmdDataOutputs amount recip opret = 
   [ H.TxOut amount $ H.encodeOutputBS recip
   , H.TxOut 0 $ H.encodeOutputBS $ H.DataCarrier opret
   ]

-- outputAmount = kmdInputAmount * (fromIntegral $ kmdNotarySigs - 1)
