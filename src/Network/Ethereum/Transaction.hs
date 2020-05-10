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


withoutSig :: Transaction -> Transaction
withoutSig tx = tx { _sig = Nothing }

encodeTx :: Transaction -> ByteString
encodeTx = rlpSerialize

decodeTx :: ByteString -> Either String Transaction
decodeTx = rlpDeserialize

hashTx :: Transaction -> Sha3
hashTx = sha3 . encodeTx

signTx :: SecKey -> Transaction -> Transaction
signTx sk tx = tx { _sig = Just (sign sk $ sighashTx tx) }

recoverFrom :: Transaction -> Maybe Address
recoverFrom tx = _sig tx >>= recoverAddr (sighashTx tx)

sighashTx :: Transaction -> Msg
sighashTx tx = fromJust $ msg $ unSha3 $ hashTx $ tx { _sig = Nothing }
