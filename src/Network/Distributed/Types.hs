{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Network.Distributed.Types where


import UnliftIO hiding (Chan)
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified StmContainers.Map as STM
import qualified Data.ByteString.Lazy as BSL
import Data.FixedBytes
import Data.Hashable
import Data.Binary
import Data.Typeable
import Data.Void
import qualified Network.Transport as NT
import GHC.Generics (Generic)


--------------------------------------------------------------------------------
-- | Monad
--------------------------------------------------------------------------------

type ProcessBase m = (MonadUnliftIO m, MonadThrow m, MonadCatch m)

class (Typeable i, ProcessBase p) => Process i p | p -> i where
  procAsk :: p (ProcessData i)

procAsks :: Process i p => (ProcessData i -> a) -> p a
procAsks f = f <$> procAsk

class Process i p => SpawnProcess m i p where
  runProcess :: p () -> RunProcess i m

type RunProcess i m = ProcessData i -> m ()

-- ReaderT process

type ProcessReaderT i m = ReaderT (ProcessData i) m

instance (ProcessBase m, Typeable i) => Process i (ProcessReaderT i m) where
  procAsk = ask

instance (ProcessBase m, Typeable i) => SpawnProcess m i (ProcessReaderT i m) where
  runProcess act r = runReaderT act r

instance (Typeable i, Typeable i2, ProcessBase m) => SpawnProcess (ProcessReaderT i2 m) i (ProcessReaderT i m) where
  runProcess act = lift . runProcess act



--------------------------------------------------------------------------------
-- | Data types
--------------------------------------------------------------------------------


newtype ProcessId = ProcessId { unProcessId :: Bytes16 }
  deriving (Show, Eq, Ord, Generic, Hashable, Binary)


newtype NodeId = NodeId { endpointAddress :: NT.EndPointAddress }
  deriving (Show, Eq, Ord, Binary)


data ProcessData a = ProcData
  { myNode  :: Node
  , myPid   :: ProcessId
  , inbox   :: TQueue a
  , typefp  :: TypeRep
  , myAsync :: Async ()
  }


-- TODO: The queue should be an IO write action rather than a queue
data ProcessHandle a = Proc
  { procChan  :: TQueue a
  , procQType :: TypeRep
  , procAsync :: Async ()
  , procId    :: ProcessId
  }

data Node = Node
  { salt :: BS.ByteString
  , transport :: NT.Transport
  , endpoint :: NT.EndPoint
  , lastPid :: TVar BS.ByteString
  , processes :: STM.Map ProcessId (ProcessHandle Void)
  }


getNodeId :: Node -> NodeId
getNodeId = NodeId . NT.address . endpoint


data ProcessNameConflict = ProcessNameConflict
  deriving (Show, Eq)

instance Exception ProcessNameConflict


