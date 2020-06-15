{-# LANGUAGE TypeFamilies #-}

module Zeno.Notariser.Targets where

import Data.Word
import Network.Bitcoin
import Network.Ethereum
import Network.Komodo
import Zeno.Consensus.Types
import Zeno.EthGateway
import Zeno.Prelude


--------------------------------------------------------------------------------
-- Common interfaces for blockchains and notarisations
--------------------------------------------------------------------------------

class BlockchainConfig c where
  getSymbol :: c -> String
  getNotarisationBlockInterval :: c -> Word32

class (BlockchainConfig c) => BlockchainAPI c m where
  getHeight :: c -> m Word32

class (BlockchainAPI c m, NotarisationReceipt (ChainNotarisationReceipt c)) => SourceChain c m where
  type ChainNotarisationReceipt c :: *
  getLastNotarisationReceipt :: c -> m (Maybe (ChainNotarisationReceipt c))

class (BlockchainAPI c m, Notarisation (ChainNotarisation c)) => DestChain c m where
  type ChainNotarisation c :: *
  getLastNotarisationAndSequence :: c -> m (Maybe (ChainNotarisation c, Int))


-- | Notarisation Data classes

class Show n => Notarisation n where
  foreignHeight :: n -> Word32

class Notarisation n => NotarisationReceipt n where
  receiptHeight :: n -> Word32




--------------------------------------------------------------------------------
-- Functions using abstract interface
--------------------------------------------------------------------------------
