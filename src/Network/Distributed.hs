
-- | Messaging and networking library, basically a cut down version of distributed-process.
--
-- The reason it was created is that the aforementioned works on the assumption that nodes
-- are to be trusted. This library distinguishes between local and remote sends.
--
-- It also does not support named sends - process IDs may be determinable or random, using
-- a salt assigned to the node. A process ID may be public knowledge, por it may remain private.


module Network.Distributed 
  ( Node(..)
  , NodeId(..)
  , Process(..)
  , ProcessData(..)
  , ProcessHandle(..)
  , ProcessId
  , RemoteMessage(..)
  , RemoteProcess
  , RemoteProcessData
  , RunProcess
  , Typeable
  , closeNode
  , getMyPid
  , getProcessById
  , getVoidProcessById
  , killProcess
  , monitorLocal
  , monitorRemote
  , nodeSpawn
  , nodeSpawn'
  , nodeSpawnNamed
  , receiveDuring
  , receiveDuringS
  , receiveMaybe
  , receiveMaybeRemote
  , receiveTimeout
  , receiveTimeoutS
  , receiveWait
  , receiveWaitRemote
  , send
  , sendRemote
  , serviceId
  , spawn
  , spawnChild
  , spawnChildNamed
  , startNode
  , withRemoteMessage
  -- Util exports
  , timeDelta
  ) where


import Data.Dynamic
import Data.Time.Clock

import Control.Monad.Reader
import Control.Concurrent.STM (check, retry)

import Network.Distributed.Types
import Network.Distributed.Remote
import Network.Distributed.Process

import qualified Network.Transport as NT

import qualified StmContainers.Map as STM
 
import System.Entropy

import UnliftIO hiding (Chan)
import UnliftIO.Concurrent hiding (Chan)

import Debug.Trace


-- TODO
-- The node should not be neccesary. The processmap should only be required
-- for remote processes. It's redundant if you only have local processes,
-- because you can just as easily store a reference to the process queue
-- as it's processId.


startNode :: NT.Transport -> IO Node
startNode transport = do
  endpoint <- NT.newEndPoint transport <&> either (error . show) id
  salt <- getEntropy 32
  lastPid <- newTVarIO ""
  processes <- STM.newIO
  let node = Node{..}
  nodeSpawn node $ runReaderT networkHandler
  pure node


closeNode :: Node -> IO ()
closeNode Node{..} = do
  NT.closeEndPoint endpoint
  NT.closeTransport transport


receiveDuring :: Process i p => Int -> (i -> p ()) -> p ()
receiveDuring timeout act = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    d <- liftIO $ timeDelta startTime
    let us = timeout - d
    receiveTimeout us >>= maybe (pure ()) act
    when (us > 0) f

receiveDuringS :: Process i p => Int -> (i -> p ()) -> p ()
receiveDuringS s = receiveDuring (s * 1000000)


spawnChild :: (SpawnProcess p i2 p2, Process i p) => p2 () -> p (ProcessHandle i2)
spawnChild act = do
  pid <- procAsks myNode >>= atomically . newPid
  spawnChildNamed pid act


spawnChildNamed :: (SpawnProcess p i2 p2, Process i p) => ProcessId -> p2 () -> p (ProcessHandle i2)
spawnChildNamed pid act = do
  parent <- procAsks myAsync
  spawnNamed pid do
    child <- procAsks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    act



receiveTimeout :: Process i p => Int -> p (Maybe i)
receiveTimeout us = do
  pd <- procAsks id
  delay <- registerDelay us
  atomically do
    receiveMaybeSTM pd >>=
      \case
        Just o -> pure (Just o)
        Nothing -> do
          readTVar delay >>= check
          pure Nothing


receiveTimeoutS :: Process i p => Int -> p (Maybe i)
receiveTimeoutS s = receiveTimeout (s * 1000000)



-- | Utils -----------


timeDelta :: MonadIO m => UTCTime -> m Int
timeDelta t = f <$> liftIO getCurrentTime where
  f now = round . (*1000000) . realToFrac $ diffUTCTime now t


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
