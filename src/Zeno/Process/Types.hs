{-# LANGUAGE RankNTypes #-}

module Zeno.Process.Types where

import UnliftIO hiding (Chan)

import Data.FixedBytes
import qualified Network.Transport as NT
import Data.Hashable
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import Data.IntMap (IntMap)
import qualified Data.ByteString as BS
import qualified StmContainers.Map as STM
import qualified Data.ByteString.Lazy as BSL
import UnliftIO

newtype ProcessId = ProcessId { unProcessId :: Bytes16 }
  deriving (Eq, Ord, Generic, Hashable, Binary)

instance Show ProcessId where
  show pid = "ProcessId " ++ show (unProcessId pid)


newtype NodeId = NodeId { endpointAddress :: NT.EndPointAddress }
  deriving (Show, Eq, Ord, Binary, Hashable)


data Node = Node
  { transport :: NT.Transport
  , endpoint :: NT.EndPoint
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
type ReceiveMiss = (ProcessId, NodeId, BSL.ByteString)
type ReceiveMissCache = IntMap ReceiveMiss

data WrappedReceiver = WrappedReceiver
  { wrappedWrite :: NodeId -> BSL.ByteString -> STM ()
  }

type Forwarder = TQueue ForwardMessage

data ForwardMessage =
    Forward BSL.ByteString
  | OnShutdown (IO ())
  | Quit

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

getNodeId :: Node -> NodeId
getNodeId = NodeId . NT.address . endpoint


data TopicIsRegistered = TopicIsRegistered
  deriving (Show, Eq)

instance Exception TopicIsRegistered

