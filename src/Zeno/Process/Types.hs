{-# LANGUAGE RankNTypes #-}

module Zeno.Process.Types where

import Crypto.Hash
import Control.Applicative
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.Serialize
import Data.String
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import qualified StmContainers.Map as STM
import Network.Simple.TCP
import Network.Socket (HostAddress)
import UnliftIO
import Zeno.Data.FixedBytes
import Zeno.Process.Node.InboundRateLimit

newtype ProcessId = ProcessId { unProcessId :: Bytes16 }
  deriving (Eq, Ord, Generic, Hashable, Serialize)

instance Show ProcessId where
  show pid = "ProcessId " ++ show (unProcessId pid)

instance IsString ProcessId where
  fromString = hashServiceId . fromString

data NodeId = NodeId
  { hostName :: !HostName
  , hostPort :: !Word16
  } deriving (Eq, Ord, Generic)

instance Show NodeId where
  show NodeId{..} = hostName ++ ":" ++ show hostPort

instance Hashable NodeId
instance Serialize NodeId

instance IsString NodeId where
  fromString s =
    let ip = takeWhile (/=':') s
        port = read $ drop (length ip + 1) s
     in NodeId ip port


data Node = Node
  { myNodeId :: NodeId
  , topics :: STM.Map ProcessId WrappedReceiver
  , mforwarders :: STM.Map NodeId Forwarder
  -- The receivers map is so that we can limit the number of incoming connections for
  -- a host. The thread reference is a mutex so that it can be synchronously killed.
  -- The reason that we kill the old connection is in case a legitimate node is
  -- reconnecting and there's a dangling TCP connection of some kind. The reason that
  -- it's killed synchronously is because doing it asynchronously (and safely) is a
  -- massive ballache of complexity and STM contention.
  , mreceivers :: ReceiverMap IO
  , missCache :: TVar ReceiveMissCache
  }

-- | The receive cache is wildly inefficient. We want to store 1-10k
--   elements that we don't have listeners for, because many nodes will
--   send data before others are listening on that key. Appending to this
--   map is cheap, but when we get a miss we have to scan it.
--   Alternatives would be: eagerly allocating a receiver for a topic we don't
--   have a listener for, and some thread to monitor it while there's no consumer,
--   or using multiple maps, one for lookup by topic and one for lookup by nonce.
type ReceiveMiss = (ProcessId, NodeId, BS.ByteString)
type ReceiveMissCache = IntMap ReceiveMiss

data WrappedReceiver = WrappedReceiver
  { wrappedWrite :: NodeId -> BS.ByteString -> STM ()
  }

type Forwarder = (TQueue ForwardMessage, TVar (IO ()))

type ForwardMessage = BSL.ByteString

type Receiver i = TQueue i

type RemoteReceiver i = Receiver (RemoteMessage i)

data RemoteMessage i = RemoteMessage
  { remoteNodeId :: NodeId
  , remoteMessage :: i
  } deriving (Typeable)


type Process i = AsyncProcess i ()

data AsyncProcess i b = Process
  { procAsync :: Async b
  , procMbox :: TBQueue i
  }

class HasReceive r i | r -> i where
  receiveSTM :: r -> STM i
  receiveMaybeSTM :: r -> STM (Maybe i)
  receiveMaybeSTM r = Just <$> receiveSTM r <|> pure Nothing

instance HasReceive (AsyncProcess i b) i where
  receiveSTM (Process{..}) = readTBQueue procMbox
  receiveMaybeSTM (Process{..}) = tryReadTBQueue procMbox

instance HasReceive (Receiver i) i where
  receiveSTM = readTQueue
  receiveMaybeSTM = tryReadTQueue

instance HasReceive (TMVar i) i where
  receiveSTM = takeTMVar
  receiveMaybeSTM = tryTakeTMVar

instance HasReceive (TBQueue i) i where
  receiveSTM = readTBQueue

data TopicIsRegistered = TopicIsRegistered ProcessId
  deriving (Show, Eq)

instance Exception TopicIsRegistered


data NetworkConfig = NetworkConfig
  { hostPref :: HostPreference
  , port :: Word16
  } deriving (Show)


blake2b_160 :: BS.ByteString -> BS.ByteString
blake2b_160 b = BS.pack (BA.unpack (hash b :: Digest Blake2b_160))

hashServiceId :: BS.ByteString -> ProcessId
hashServiceId = ProcessId . toFixed . blake2b_160 
