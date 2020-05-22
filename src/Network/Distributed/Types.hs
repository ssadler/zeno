{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

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

--type ProcessBase m = (MonadUnliftIO m, MonadThrow m, MonadCatch m)
type ProcessBase m = MonadUnliftIO m

class (Typeable i, ProcessBase (p i m), ProcessBase m) => MonadProcess i m p where
  procAsk :: p i m (ProcessData i)
  procLift :: MonadProcess i m p => m a -> p i m a
  procRun :: p i m () -> ProcessData i -> m ()

procAsks :: MonadProcess i m p => (ProcessData i -> a) -> p i m a
procAsks f = f <$> procAsk

type RunProcess i m = ProcessData i -> m ()

class (MonadProcess i m p, MonadProcess i2 m p) => ForkProcess p i i2 m where
  procFork :: p i2 m () -> ProcessData i2 -> p i m ()

-------------------------------------------------------------------------------
-- | ProcessT
-------------------------------------------------------------------------------

newtype ProcessT i m a = ProcessT (ReaderT (ProcessData i) m a)
  deriving (Functor, Applicative, Monad, MonadReader (ProcessData i)
           , MonadTrans, MonadIO)

runProcessT :: ProcessT i m a -> ProcessData i -> m a
runProcessT (ProcessT act) = runReaderT act

instance MonadIO m => MonadUnliftIO (ProcessT i m)
-- instance MonadThrow m => MonadThrow (ProcessT i m)
-- instance MonadCatch m => MonadCatch (ProcessT i m)

instance (ProcessBase m, Typeable i) => MonadProcess i m ProcessT where
  procAsk = ask
  procLift = lift
  procRun (ProcessT ma) = runReaderT ma

instance (Typeable i, Typeable i2, ProcessBase m)
         => ForkProcess ProcessT i i2 m where
  procFork (ProcessT act) r = lift $ runReaderT act r


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

