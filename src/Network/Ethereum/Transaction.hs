module Network.Ethereum.Transaction
  ( module ALL
  , encodeTx
  , signTx
  , recoverFrom
  , txid
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

txid :: Transaction -> Sha3
txid = sha3 . encodeTx . withoutSig

-- TODO: flip arguments
signTx :: Transaction -> SecKey -> Transaction
signTx tx sk = 
  let Just payload = msg $ unSha3 $ txid tx
      sig = sign sk payload
   in tx { _sig = Just sig }

recoverFrom :: Transaction -> Maybe Address
recoverFrom tx = do
  message <- msg $ unSha3 $ txid tx
  _sig tx >>= recoverAddr message

