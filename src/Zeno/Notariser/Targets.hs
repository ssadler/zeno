{-# LANGUAGE TypeFamilies #-}

module Zeno.Notariser.Targets where

import Data.Word
import Network.Bitcoin
import Network.Ethereum
import Network.Komodo
import Zeno.Consensus.Types
import Zeno.EthGateway
import Zeno.Monad


class BlockChain c m where
  type ChainNotarisation c :: *
  waitHeight :: c -> Word32 -> m ()

class SourceChain c m where
  getLastNotarisation :: c -> m (Maybe (ChainNotarisation c))

class DestChain c m where
  getLastNotarisationAndSequence :: c -> m (Maybe (ChainNotarisation c, Int))


data KMDSource = KMDSource
  { kmdSymbol :: String
  } deriving (Show, Eq)

instance BlockChain KMDSource (Zeno r) where
  type (ChainNotarisation KMDSource) = KomodoNotarisationReceipt
  waitHeight KMDSource{..} = error "KMDSource waitheight"

instance Has BitcoinConfig r => SourceChain KMDSource (Zeno r) where
  getLastNotarisation KMDSource{..} = kmdGetLastNotarisationData kmdSymbol


data ETHDest = ETHDest
  { ethChainId :: ChainId
  , ethSymbol :: String
  , ethNotarisationsContract :: Address
  } deriving (Show, Eq)

instance BlockChain ETHDest (Zeno r) where
  type (ChainNotarisation ETHDest) = EthNotarisationData
  waitHeight ETHDest{..} = error "ETHDest waitheight"

instance Has GethConfig r => DestChain ETHDest (Zeno r) where
  getLastNotarisationAndSequence ETHDest{..} = do
    ethGetLastNotarisationAndSequence ethNotarisationsContract

