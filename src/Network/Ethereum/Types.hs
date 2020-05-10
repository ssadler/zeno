
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
  , logsBloom :: Hex
  , logs :: [LogEntry]
  } deriving (Generic, Show)

instance FromJSON TransactionReceipt

instance RLPEncodable TransactionReceipt where
  rlpEncode Receipt{..} = RLP.Array $
    [ maybe (rlpEncode $ fromJust status) rlpEncode root
    , rlpEncode cumulativeGasUsed
    , rlpEncode logsBloom
    , rlpEncode logs
    ]
  rlpDecode _ = Left "rlpDecode TransactionReceipt undefined"


data LogEntry = LogEntry
  { logAddress :: Address
  , logTopics :: [Sha3]
  , logData :: Hex
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
    , rlpEncode logData
    ]
  rlpDecode _ = Left "rlpDecode LogEntry undefined"


--
-- void TransactionReceipt::streamRLP(RLPStream& _s) const
-- {                                                      
--     _s.appendList(4);                                  
--     if (hasStatusCode())                               
--         _s << statusCode();                            
--     else                                               
--         _s << stateRoot();                             
--     _s << m_gasUsed << m_bloom;                        
--     _s.appendList(m_log.size());                       
--     for (LogEntry const& l: m_log)                     
--         l.streamRLP(_s);                               
-- }                                                      
-- {
--   "contractAddress": null,
--   "logsBloom": "0x00000000000400000000010000400000000000000000040000010000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000008000000000040000000000000000000000000000000000000000000000000000000000000000000000000000000001010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000100002000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000",
--   "status": "0x1",
--   "gasUsed": "0xcb20",
--   "blockHash": "0x75f9ec50173bfbdb71737ec26f5cc08a98d80eff289978343a0677cc773e2101",
--   "transactionHash": "0x80703eba27d9e0c6b1e19ef847d3b4aab7f04da4c510639c742591f8eef724e2",
--   "root": null,
--   "cumulativeGasUsed": "0x76b6e6",
--   "logs": [
--     {
--       "topics": [
--         "0xddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef",
--         "0x000000000000000000000000989fcbc46845a290e971a6303ef3753fb039d8d5",
--         "0x000000000000000000000000a98eff8c8a3bd7ce80446168aa09aaf31f0ab072"
--       ],
--       "logIndex": "0x78",
--       "data": "0x0000000000000000000000000000000000000000000000000000048c27395000",
--       "blockHash": "0x75f9ec50173bfbdb71737ec26f5cc08a98d80eff289978343a0677cc773e2101",
--       "transactionHash": "0x80703eba27d9e0c6b1e19ef847d3b4aab7f04da4c510639c742591f8eef724e2",
--       "address": "0x06bead2ead661b51307b646f7419d5284330c135",
--       "transactionLogIndex": "0x0",
--       "blockNumber": "0x5f0134",
--       "transactionIndex": "0x5f",
--       "type": "mined"
--     }
--   ],
--   "blockNumber": "0x5f0134",
--   "transactionIndex": "0x5f"
-- }

