
module Network.Ethereum.Types where

import           GHC.Generics
import           Data.RLP as RLP

import           Network.Ethereum.Data
import           Network.Ethereum.Crypto

import           Zeno.Prelude
import           Zeno.Data.Aeson


newtype GethConfig = GethConfig { gethEndpoint :: String }
  deriving (Show)

data EthBlock = EthBlock
  { ethBlockNumber :: U256
  , ethBlockHash :: Sha3
  , ethBlockReceiptsRoot :: Sha3
  , ethBlockTransactions :: [Sha3]
  } deriving (Show)

instance FromJSON EthBlock where
  parseJSON val = do
    obj <- parseJSON val
    EthBlock <$> obj .: "number"
             <*> obj .: "hash"
             <*> obj .: "receiptsRoot"
             <*> obj .: "transactions"



data TransactionReceipt = Receipt
  { blockHash :: Sha3
  , transactionIndex :: U256
  , gasUsed :: U256
  , cumulativeGasUsed :: U256
  , root :: Maybe Sha3
  , status :: Maybe U256
  , logsBloom :: PrefixedHex ByteString
  , logs :: [LogEntry]
  } deriving (Generic, Show)

instance FromJSON TransactionReceipt

instance RLPEncodable TransactionReceipt where
  rlpEncode Receipt{..} = RLP.Array $
    [ maybe (rlpEncode $ fromJust status) rlpEncode root
    , rlpEncode cumulativeGasUsed
    , rlpEncode (unPrefixedHex logsBloom)
    , rlpEncode logs
    ]
  rlpDecode _ = Left "rlpDecode TransactionReceipt undefined"


data LogEntry = LogEntry
  { logAddress :: Address
  , logTopics :: [Sha3]
  , logData :: PrefixedHex ByteString
  } deriving (Show)

instance FromJSON LogEntry where
  parseJSON val = do
    obj <- parseJSON val
    LogEntry <$> obj .: "address"
             <*> obj .: "topics"
             <*> obj .: "data"

instance RLPEncodable LogEntry where
  rlpEncode LogEntry{..} = RLP.Array $
    [ rlpEncode logAddress
    , rlpEncode logTopics
    , rlpEncode (unPrefixedHex logData)
    ]
  rlpDecode _ = Left "rlpDecode LogEntry undefined"

