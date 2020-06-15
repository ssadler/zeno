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
hashTx = PrefixedHex . sha3b . encodeTx

signTx :: MonadUnliftIO m => SecKey -> Transaction -> m Transaction
signTx sk tx = do
  sig <- signIO sk $ unPrefixedHex $ sighashTx tx
  pure $ tx { _sig = Just sig }

recoverFrom :: MonadUnliftIO m => Transaction -> m (Maybe Address)
recoverFrom tx =
  case _sig tx of
    Nothing -> pure Nothing
    Just sig -> do
      recoverAddr (unPrefixedHex $ sighashTx tx) sig >>= pure . Just

sighashTx :: Transaction -> EthTxHash
sighashTx tx = hashTx $ tx { _sig = Nothing }
