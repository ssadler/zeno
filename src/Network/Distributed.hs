{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}


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
  , ProcessId
  , RunProcess
  , Typeable
  , RemoteMessage(..)
  , closeNode
  , getMyPid
  , monitorRemote
  , nodeSpawn
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteArray as BA
import Data.Maybe
import Data.Dynamic
import Data.Hashable (Hashable)
import Data.FixedBytes
import Control.Exception (AsyncException(..))
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.STM (check, retry)

import Crypto.Hash

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary
import Data.Time.Clock
import qualified StmContainers.Map as STM

import GHC.Generics (Generic)

import qualified Network.Transport as NT

import System.Entropy

import UnliftIO hiding (Chan)
import UnliftIO.Concurrent hiding (Chan)


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
  { node    :: Node
  , myPid   :: ProcessId
  , inbox   :: TQueue Dynamic
  , myAsync :: Async ()
  }


-- TODO: The queue should be an IO write action rather than a queue
data ProcessHandle = Proc
  { procChan :: TQueue Dynamic
  , procAsync :: Async ()
  }


data Node = Node
  { salt :: BS.ByteString
  , transport :: NT.Transport
  , endpoint :: NT.EndPoint
  , lastPid :: TVar BS.ByteString
  , processes :: STM.Map ProcessId ProcessHandle
  }



data RemoteMessage = RemoteMessage NodeId BSL.ByteString
  deriving (Typeable)


data ProcessNameConflict = ProcessNameConflict
  deriving (Show, Eq)

instance Exception ProcessNameConflict


--------------------------------------------------------------------------------
-- | 
--------------------------------------------------------------------------------


startNode :: NT.Transport -> IO Node
startNode transport = do
  endpoint <- NT.newEndPoint transport <&> either (error . show) id
  salt <- getEntropy 32
  lastPid <- newTVarIO ""
  processes <- STM.newIO
  let node = Node{..}
  nodeSpawn node $ runReaderT networkHandler
  pure node



getMyPid :: Process p => p ProcessId
getMyPid = procAsks myPid


newPid :: Node -> STM ProcessId
newPid node@Node{..} = do
  lp <- readTVar lastPid
  let np = blake2b_160 (salt <> lp)
  writeTVar lastPid np
  pure $ ProcessId $ toFixed np


getProcessById :: Node -> ProcessId -> STM (Maybe ProcessHandle)
getProcessById Node{..} pid = do
  STM.lookup pid processes


nodeSpawn :: ProcessBase m => Node -> RunProcess m -> m ProcessId
nodeSpawn node act = do
  pid <- atomically $ newPid node
  nodeSpawnNamed node pid act
  pure pid


nodeSpawnNamed :: ProcessBase m => Node -> ProcessId -> RunProcess m -> m ProcessHandle
nodeSpawnNamed node@Node{processes} pid act = do
  atomically (STM.lookup pid processes) >>=
    maybe (pure ()) (\_ -> throw ProcessNameConflict)

  chan <- newTQueueIO
  handoff <- newEmptyTMVarIO

  async' <- async do
    r <- ProcData node pid chan <$> atomically (takeTMVar handoff)
    act r

  let proc = Proc chan async'

  atomically do
    STM.insert proc pid processes
    putTMVar handoff async'

  pure proc


spawn :: Process p => p () -> p ProcessId
spawn act = do
  procAsks node >>= \n -> nodeSpawn n (flip procWith act)


spawnNamed :: Process p => ProcessId -> p () -> p ()
spawnNamed pid act = do
  procAsks node >>= \node -> nodeSpawnNamed node pid (flip procWith act)
  pure ()


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


-- Spawns a process and links it to it's parent so that
-- it will die when it's parent dies
spawnChild :: Process p => p () -> p ProcessId
spawnChild act = do
  parent <- procAsks myAsync
  spawn do
    child <- procAsks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    act

-- TODO: DRY
spawnChildNamed :: Process p => ProcessId -> p () -> p ()
spawnChildNamed pid act = do
  parent <- procAsks myAsync
  spawnNamed pid do
    child <- procAsks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    act



monitorRemote :: Process p => NodeId -> p () -> p ()
monitorRemote nodeId onTerminate = do
  node@Node{..} <- procAsks node
  -- TODO: atomic, mask, etc
  (pid, Proc{..}) <- liftIO $ getForwarder node nodeId
  spawn do
    waitCatch procAsync >>= \_ -> onTerminate
  pure ()



killProcess :: MonadIO m => Node -> ProcessId -> m ()
killProcess node pid = do
  atomically (getProcessById node pid) >>=
    \case
      Nothing -> pure ()
      Just Proc{..} -> uninterruptibleCancel procAsync


closeNode :: Node -> IO ()
closeNode Node{..} = do
  NT.closeEndPoint endpoint
  NT.closeTransport transport



say :: Process p => String -> p ()
say = error "say"

send :: (Process p, Typeable a) => ProcessId -> a -> p ()
send pid msg = do
  node <- procAsks node
  atomically $ sendSTM node pid msg


sendSTM :: Typeable a => Node -> ProcessId -> a -> STM ()
sendSTM node pid msg = do
  getProcessById node pid >>=
    \case
      Nothing -> pure ()
      Just proc -> sendProcSTM proc msg


sendProcSTM :: Typeable a => ProcessHandle -> a -> STM ()
sendProcSTM Proc{..} = writeTQueue procChan . toDyn

data SendToPeer = SendToPeer ProcessId BSL.ByteString
  deriving (Typeable, Generic)

instance Binary SendToPeer


-- | Each peer connection has a local forwarder process.
--   The forwarder process has a salted key so it can't be
--   guessed by other nodes. When it's attached connection goes down,
--   it gets killed. Other processes can listen for it's demise to 
--   perform cleanups.
getForwarder :: Node -> NodeId -> IO (ProcessId, ProcessHandle)
getForwarder node@Node{..} nodeId = do
  let pid = getForwarderId salt nodeId

  -- TODO: this needs to be atomic. withAtomicIO :: Node -> ...
  mproc <- atomically $ getProcessById node pid
  proc <- maybe (nodeSpawnNamed node pid run) pure mproc

  pure (pid, proc)

  where
  run = runReaderT do
    liftIO (setupConn endpoint nodeId) >>=
      \case Nothing   -> pure ()                -- TODO: log?
            Just conn -> loopHandle conn

  loopHandle conn = do
    receiveWait >>=
      -- This is all wrong. It should be a RemoteMessage
      \pm@(SendToPeer _ _) -> do
        _ <- liftIO $ NT.send conn (BSL.toChunks $ encode pm) -- TODO: failure? Die on failure
        loopHandle conn


getForwarderId :: BS.ByteString -> NodeId -> ProcessId
getForwarderId salt nodeId = 
  ProcessId $ toFixed $ blake2b_160 $ salt <> BS8.pack (show nodeId)


setupConn :: NT.EndPoint -> NodeId -> IO (Maybe NT.Connection)
setupConn endpoint nodeId = do
  mConn <- NT.connect endpoint
                      (endpointAddress nodeId)
                      NT.ReliableOrdered
                      NT.defaultConnectHints
  case mConn of
    Left _ -> pure Nothing
    Right conn -> do
      didSend <- NT.send conn (BSL.toChunks $ encode ())
      case didSend of
        Left _   -> pure Nothing
        Right () -> pure $ Just conn




sendRemote :: (Binary a, Process p) => NodeId -> ProcessId -> a -> p ()
sendRemote nodeId theirPid msg = do
  node <- procAsks node
  (_, proc) <- liftIO $ getForwarder node nodeId
  atomically $
    -- TODO: RemoteMessage??
    sendProcSTM proc $ SendToPeer theirPid $ encode msg


receiveMaybe :: (Process p, Typeable a) => p (Maybe a)
receiveMaybe = receiveTimeout 0


receiveWait :: (Process p, Typeable a) => p a
receiveWait = do
  r <- procAsks inbox >>= atomically . readTQueue
  case fromDynamic r of
    Nothing -> receiveWait
    Just a -> pure a


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




-- | Network Event Handler

networkHandler :: ReaderT ProcessData IO ()
networkHandler = do
  node@Node{..} <- procAsks node

  fix1 mempty $
    \go !conns -> do

      evt <- liftIO $ NT.receive endpoint
      case evt of
        NT.ConnectionOpened connId _ theirEndpoint -> do
          go $ Map.insert connId (NodeId theirEndpoint) conns

        NT.Received connId bss -> do
          case Map.lookup connId conns of
            Nothing -> do
              go conns                                        --  TODO: log very bad thing?
            Just nodeId -> do
              let bs = BSL.fromChunks bss
              case decodeOrFail (BSL.fromChunks bss) of
                Left (_, _, errStr) -> go conns               -- TODO: log?
                Right (rem, _, to) -> do
                  send to $ RemoteMessage nodeId rem
                  go conns

        NT.ErrorEvent (NT.TransportError err description) -> do
          case err of
            NT.EventEndPointFailed -> error "Unrecoverable: Local endpoint failed"   -- TODO: panic
            NT.EventTransportFailed -> error "Unrecoverable: Local transport failed" -- TODO: panic
            NT.EventConnectionLost addr -> do
              let nodeId = NodeId addr
              killProcess node $ getForwarderId salt nodeId
              go $ Map.filter (== nodeId) conns

        -- TODO: Maybe we don't need to run killProcess on
        -- ConnectionClosed and EventConnectionLost. One should imply the other, which comes first?
        NT.ConnectionClosed connId -> do
          case Map.lookup connId conns of
            Nothing -> pure ()
            Just nodeId -> do
              killProcess node $ getForwarderId salt nodeId
              go $ Map.delete connId conns

        NT.ReceivedMulticast _ _ -> go conns              -- TODO: log?
        NT.EndPointClosed -> pure ()                      -- TODO: log



withRemoteMessage :: (Process p, Binary i) => (NodeId -> i -> p ()) -> RemoteMessage -> p ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  case decodeOrFail bs of
    Right ("", _, a) -> act nodeId a
    Right (_, _, _) -> pure () -- TODO: log
    Left _ -> pure () -- TODO: log

    



-- | Utils -----------


timeDelta :: MonadIO m => UTCTime -> m Int
timeDelta t = f <$> liftIO getCurrentTime where
  f now = round . (*1000000) . realToFrac $ diffUTCTime now t


blake2b_160 :: BS.ByteString -> BS.ByteString
blake2b_160 b = BS.pack (BA.unpack (hash b :: Digest Blake2b_160))


serviceId :: BS.ByteString -> ProcessId
serviceId = ProcessId . toFixed . blake2b_160


fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
infixl 1 <&>
