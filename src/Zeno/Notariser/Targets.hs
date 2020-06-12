{-# LANGUAGE TypeFamilies #-}

module Zeno.Notariser.Targets where

import Data.Word
import Network.Bitcoin
import Network.Ethereum
import Network.Komodo
import Zeno.Consensus.Types
import Zeno.EthGateway
import Zeno.Prelude


-- | Blockchain interface classes

class BlockchainConfig c where
  getSymbol :: c -> String

class (BlockchainConfig c) => Blockchain c m where
  waitHeight :: c -> Word32 -> m ()

class (Blockchain c m, NotarisationReceipt (ChainNotarisationReceipt c)) => SourceChain c m where
  type ChainNotarisationReceipt c :: *
  getLastNotarisationReceipt :: c -> m (Maybe (ChainNotarisationReceipt c))

class (Blockchain c m, Notarisation (ChainNotarisation c)) => DestChain c m where
  type ChainNotarisation c :: *
  getLastNotarisationAndSequence :: c -> m (Maybe (ChainNotarisation c, Int))


-- | Notarisation Data classes

class Show n => Notarisation n where
  foreignHeight :: n -> Word32

class Show n => NotarisationReceipt n where
  receiptHeight :: n -> Word32
