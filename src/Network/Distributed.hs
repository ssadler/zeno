
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
  , RunProcess
  , Typeable
  , RemoteMessage(..)
  , closeNode
  , getMyPid
  , getProcessById
  , killProcess
  , monitorRemote
  , nodeSpawn
  , nodeSpawn'
  , nodeSpawnNamed
  , receiveDuring
  , receiveDuringS
  , receiveMaybe
  , receiveTimeout
  , receiveTimeoutS
  , receiveWait
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


receiveDuring :: (Process p, Typeable a) => Int -> (a -> p ()) -> p ()
receiveDuring timeout act = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    d <- liftIO $ timeDelta startTime
    let us = timeout - d
    when (us > 0) $
       receiveTimeout us >>= maybe (pure ()) act

receiveDuringS :: (Process p, Typeable a) => Int -> (a -> p ()) -> p ()
receiveDuringS s = receiveDuring (s * 1000000)


spawnChild :: Process p => p () -> p ProcessHandle
spawnChild act = do
  pid <- procAsks node >>= atomically . newPid
  spawnChildNamed pid act


spawnChildNamed :: Process p => ProcessId -> p () -> p ProcessHandle
spawnChildNamed pid act = do
  parent <- procAsks myAsync
  spawnNamed pid do
    child <- procAsks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    act




closeNode :: Node -> IO ()
closeNode Node{..} = do
  NT.closeEndPoint endpoint
  NT.closeTransport transport


receiveMaybe :: (Process p, Typeable a) => p (Maybe a)
receiveMaybe = receiveTimeout 0


-- TODO: Does this function peg CPU?
receiveTimeout :: (Process p, Typeable a) => Int -> p (Maybe a)
receiveTimeout us = do
  ProcData{..} <- procAsks id
  delay <- registerDelay us
  atomically do
    tryReadTQueue inbox >>=
      \case
        Nothing -> do
          readTVar delay >>= check
          pure Nothing
        Just d ->
          case fromDynamic d of
            Nothing -> retry
            Just msg -> pure (Just msg)

receiveTimeoutS :: (Process p, Typeable a) => Int -> p (Maybe a)
receiveTimeoutS s = receiveTimeout (s * 1000000)



-- | Utils -----------


timeDelta :: MonadIO m => UTCTime -> m Int
timeDelta t = f <$> liftIO getCurrentTime where
  f now = round . (*1000000) . realToFrac $ diffUTCTime now t


(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
