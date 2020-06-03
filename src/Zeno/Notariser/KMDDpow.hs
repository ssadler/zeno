
module Zeno.Notariser.KMDDpow where

import Data.Serialize

import qualified Haskoin as H

import Network.Komodo

import Zeno.Notariser.Interfaces.KMD
import Zeno.Notariser.Types
import Zeno.Prelude


notariseKmdDpow :: NotariserConfig -> NotarisationData -> Zeno EthNotariser ()
notariseKmdDpow nc@NotariserConfig{..} ndata = do
  txhash <- notariseToKMD nc "eth ⇒  kmd" outputs
  dpowCheck nc txhash ndata
  where
  outputs = kmdDataOutputs nc dpowRecip $ encode ndata
  dpowRecip = H.PayPK "020e46e79a2a8d12b9b5d12c7a91adb4e454edfae43c0a0cb805427d2ac7613fd9"
                      -- addr is RXL3YXG2ceaB6C5hfJcN4fvmLH2C34knhA
