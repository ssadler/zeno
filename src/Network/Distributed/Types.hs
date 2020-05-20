{-# LANGUAGE ConstraintKinds #-}

module Network.Distributed.Types where


import UnliftIO hiding (Chan)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified StmContainers.Map as STM
import qualified Data.ByteString.Lazy as BSL
import Data.Dynamic
import Data.FixedBytes
import Data.Hashable
import Data.Binary
import qualified Network.Transport as NT
import GHC.Generics (Generic)


--------------------------------------------------------------------------------
-- | Monad
--------------------------------------------------------------------------------

type ProcessBase m = (MonadUnliftIO m, MonadThrow m, MonadCatch m)

class ProcessBase p => Process p where
  procAsks :: (ProcessData -> a) -> p a
  procWith :: ProcessData -> p () -> p ()

instance Process (ReaderT ProcessData IO) where
  procAsks = asks
  procWith r p = withReaderT (const r) p


type RunProcess m = ProcessData -> m ()


--------------------------------------------------------------------------------
-- | Data types
--------------------------------------------------------------------------------


newtype ProcessId = ProcessId { unProcessId :: Bytes16 }
  deriving (Show, Eq, Ord, Generic, Hashable, Binary)


newtype NodeId = NodeId { endpointAddress :: NT.EndPointAddress }
  deriving (Show, Eq, Ord, Binary)


data ProcessData = ProcData
  { myNode    :: Node
  , myPid   :: ProcessId
  , inbox   :: TQueue Dynamic
  , myAsync :: Async ()
  }


-- TODO: The queue should be an IO write action rather than a queue
data ProcessHandle = Proc
  { procChan :: TQueue Dynamic
  , procAsync :: Async ()
  , procId :: ProcessId
  }


data Node = Node
  { salt :: BS.ByteString
  , transport :: NT.Transport
  , endpoint :: NT.EndPoint
  , lastPid :: TVar BS.ByteString
  , processes :: STM.Map ProcessId ProcessHandle
  }


getNodeId :: Node -> NodeId
getNodeId = NodeId . NT.address . endpoint


data ProcessNameConflict = ProcessNameConflict
  deriving (Show, Eq)

instance Exception ProcessNameConflict


