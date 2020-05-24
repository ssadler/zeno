{-# LANGUAGE RankNTypes #-}

module Zeno.Process.Types where

import UnliftIO hiding (Chan)

import Data.FixedBytes
import qualified Network.Transport as NT
import Data.Hashable
import Data.Binary
import Data.Typeable
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified StmContainers.Map as STM
import qualified Data.ByteString.Lazy as BSL
import UnliftIO

newtype ProcessId = ProcessId { unProcessId :: Bytes16 }
  deriving (Show, Eq, Ord, Generic, Hashable, Binary)


newtype NodeId = NodeId { endpointAddress :: NT.EndPointAddress }
  deriving (Show, Eq, Ord, Binary, Hashable)


data Node = Node
  { transport :: NT.Transport
  , endpoint :: NT.EndPoint
  , topics :: STM.Map ProcessId WrappedReceiver
  , mforwarders :: STM.Map NodeId Forwarder
  }

data WrappedReceiver = WrappedReceiver
  { wrappedWrite :: NodeId -> BSL.ByteString -> IO ()
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

