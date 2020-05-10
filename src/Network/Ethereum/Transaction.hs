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

-- TODO: flip arguments
signTx :: Transaction -> SecKey -> Transaction
signTx tx sk = 
  let Just payload = msg $ unSha3 $ hashTx tx
      sig = sign sk payload
   in tx { _sig = Just sig }

recoverFrom :: Transaction -> Maybe Address
recoverFrom tx = do
  message <- msg $ unSha3 $ hashTx tx
  _sig tx >>= recoverAddr message

