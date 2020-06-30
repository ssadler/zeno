module Network.Ethereum.Transaction
  ( module ALL
  , encodeTx
  , decodeTx
  , signTx
  , hashTx
  , recoverFrom
  , withoutSig
  ) where

import Data.RLP
import Network.Ethereum.Crypto
import Network.Ethereum.Transaction.Types as ALL

import Zeno.Prelude
import UnliftIO


withoutSig :: Transaction -> Transaction
withoutSig tx = tx { _sig = Nothing }

encodeTx :: Transaction -> ByteString
encodeTx = rlpSerialize

decodeTx :: ByteString -> Either String Transaction
decodeTx = rlpDeserialize

hashTx :: Transaction -> EthTxHash
hashTx = PrefixedHash . sha3b . encodeTx

signTx :: SecKey -> Transaction -> Transaction
signTx sk tx =
  let sig = sign sk $ unPrefixedHash $ sighashTx tx
   in tx { _sig = Just sig }

recoverFrom :: Transaction -> Either String Address
recoverFrom tx = do
  sig <- maybe (Left "not signed") Right (_sig tx)
  recoverAddr (unPrefixedHash $ sighashTx tx) sig

sighashTx :: Transaction -> EthTxHash
sighashTx tx = hashTx $ tx { _sig = Nothing }
