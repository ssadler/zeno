{-# LANGUAGE RankNTypes #-}

module Zeno.Process.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import Data.IntMap (IntMap)
import Data.Serialize
import Data.String
import Data.Typeable
import Data.Word
import GHC.Generics (Generic)
import qualified StmContainers.Map as STM
import Network.Simple.TCP
import UnliftIO
import Zeno.Data.FixedBytes

newtype ProcessId = ProcessId { unProcessId :: Bytes16 }
  deriving (Eq, Ord, Generic, Hashable, Serialize)

instance Show ProcessId where
  show pid = "ProcessId " ++ show (unProcessId pid)

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
  { ourPort :: Word16
  , topics :: STM.Map ProcessId WrappedReceiver
  , mforwarders :: STM.Map NodeId Forwarder
  , recvCache :: TVar ReceiveMissCache
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
  , procMbox :: TMVar i
  }

class HasReceive r i | r -> i where
  receiveSTM :: r -> STM i
  receiveMaybeSTM :: r -> STM (Maybe i)

instance HasReceive (AsyncProcess i b) i where
  receiveSTM (Process{..}) = takeTMVar procMbox
  receiveMaybeSTM (Process{..}) = tryTakeTMVar procMbox

instance HasReceive (Receiver i) i where
  receiveSTM = readTQueue
  receiveMaybeSTM = tryReadTQueue

instance HasReceive (TMVar i) i where
  receiveSTM = takeTMVar
  receiveMaybeSTM = tryTakeTMVar

data TopicIsRegistered = TopicIsRegistered ProcessId
  deriving (Show, Eq)

instance Exception TopicIsRegistered


data NetworkConfig = NC
  { hostPref :: HostPreference
  , port :: Word16
  } deriving (Show)

