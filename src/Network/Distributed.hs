{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BangPatterns #-}

module Network.Distributed 
  ( Node
  , NodeId(..)
  , Process
  , ProcessId(..)
  , Typeable
  , RemoteMessage(..)
  , RemoteProcessId(..)
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
  , remoteServiceId
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
import Control.Exception (AsyncException(..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.STM (check, retry)

import Crypto.Hash
import Lens.Micro.Platform

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary
import Data.Time.Clock

import GHC.Generics (Generic)

import qualified Network.Transport as NT

import System.Entropy

import UnliftIO hiding (Chan)
import UnliftIO.Concurrent hiding (Chan)



newtype NodeId = NodeId { endpointAddress :: NT.EndPointAddress }
  deriving (Show, Eq, Ord, Binary)


-- TODO: The queue should be an IO write action rather than a queue
data Proc = Proc
  { procChan :: TQueue Dynamic
  , procAsync :: Async ()
  } 


-- https://github.com/nikita-volkov/stm-containers
data InnerNode = Inner
  { _lastPid :: BS.ByteString
  , _processes :: Map.Map ProcessId Proc
  }

data Node = Node
  { salt :: BS.ByteString
  , transport :: NT.Transport
  , endpoint :: NT.EndPoint
  , tInnerNode :: TVar InnerNode
  }

data ProcData = PD
  { node    :: Node
  , myPid   :: ProcessId
  , inbox   :: TQueue Dynamic
  , myAsync :: Async ()
  }



data RemoteMessage = RemoteMessage NodeId BSL.ByteString
  deriving (Typeable)


newtype Process a = Process { unProcess :: ReaderT ProcData IO a }
  deriving ( Functor, Monad, Applicative
           , MonadIO, MonadThrow, MonadCatch
           , MonadReader ProcData, MonadUnliftIO
           )


newtype ProcessId = ProcessId BS.ByteString
  deriving (Show, Eq, Ord, Generic)

instance Binary ProcessId

data RemoteProcessId = RemoteProcessId
  { remoteNodeId :: NodeId
  , remotePid :: ProcessId
  } deriving (Show, Eq, Ord, Generic)

instance Binary RemoteProcessId


makeLenses ''InnerNode


startNode :: NT.Transport -> IO Node
startNode transport = do
  endpoint <- NT.newEndPoint transport <&> either (error . show) id
  salt <- getEntropy 32
  let _lastPid = ""
      _processes = mempty
  tInnerNode <- newTVarIO Inner{..}
  let node = Node{..}
  nodeSpawn node networkHandler
  pure node


readInnerNode :: Node -> STM InnerNode
readInnerNode = readTVar . tInnerNode

modifyInnerNode :: Node -> (InnerNode -> (InnerNode, a)) -> STM a
modifyInnerNode Node{..} f = do
  r <- readTVar tInnerNode
  let (r', o) = f r
  writeTVar tInnerNode r'
  pure o

modifyInnerNode_ :: Node -> (InnerNode -> InnerNode) -> STM ()
modifyInnerNode_ node f = modifyInnerNode node $ \i -> (f i, ())



getMyPid :: Process ProcessId
getMyPid = asks myPid

nextProcessId :: Node -> STM ProcessId
nextProcessId node@Node{..} = do
  modifyInnerNode node $
    \inner -> 
      let (p, inner') = inner & lastPid <%~ perm
       in (inner', ProcessId p)
  where
  perm bs = blake2b (salt <> bs)

getProcessById :: Node -> ProcessId -> STM (Maybe Proc)
getProcessById node pid = do
  Inner{..} <- readInnerNode node
  pure $ view (at pid) _processes

nodeSpawn :: MonadIO m => Node -> Process () -> m ProcessId
nodeSpawn node act = do
  pid <- atomically $ nextProcessId node
  nodeSpawnNamed node pid act
  pure pid

nodeSpawnNamed :: MonadIO m => Node -> ProcessId -> Process () -> m Proc
nodeSpawnNamed node pid act = do
  chan <- newTQueueIO

  t <- atomically $ newEmptyTMVar
  async' <- liftIO $ async do
    r <- PD node pid chan <$> atomically (takeTMVar t)
    runReaderT (unProcess act) r

  let proc = Proc chan async'

  atomically do
    modifyInnerNode_ node $ processes . at pid .~ Just proc
    putTMVar t async'

  pure proc



spawn :: Process () -> Process ProcessId
spawn act = do
  asks node >>= flip nodeSpawn act

spawnNamed :: ProcessId -> Process () -> Process ()
spawnNamed pid act = do
  asks node >>= \node -> nodeSpawnNamed node pid act
  pure ()



receiveDuring :: Typeable a => Int -> (a -> Process ()) -> Process ()
receiveDuring timeout act = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    d <- liftIO $ timeDelta startTime
    let us = timeout - d
    when (us > 0) $
       receiveTimeout us >>= maybe (pure ()) act

receiveDuringS :: Typeable a => Int -> (a -> Process ()) -> Process ()
receiveDuringS s = receiveDuring (s * 1000000)


-- Spawns a process and links it to it's parent so that
-- it will die when it's parent dies
spawnChild :: Process () -> Process ProcessId
spawnChild act = do
  parent <- asks myAsync
  spawn do
    child <- asks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    act

-- TODO: DRY
spawnChildNamed :: ProcessId -> Process () -> Process ()
spawnChildNamed pid act = do
  parent <- asks myAsync
  spawnNamed pid do
    child <- asks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    act



monitorRemote :: NodeId -> Process () -> Process ()
monitorRemote nodeId onTerminate = do
  node@Node{..} <- asks node
  -- TODO: atomic, mask, etc
  (pid, Proc{..}) <- getForwarder node nodeId
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



say :: String -> Process ()
say = error "say"

send :: Typeable a => ProcessId -> a -> Process ()
send pid msg = do
  node <- asks node
  atomically $ sendSTM node pid msg


sendSTM :: Typeable a => Node -> ProcessId -> a -> STM ()
sendSTM node pid msg = do
  getProcessById node pid >>=
    \case
      Nothing -> pure ()
      Just proc -> sendProcSTM proc msg


sendProcSTM :: Typeable a => Proc -> a -> STM ()
sendProcSTM Proc{..} = writeTQueue procChan . toDyn

data SendToPeer = SendToPeer ProcessId BSL.ByteString
  deriving (Typeable, Generic)

instance Binary SendToPeer


-- | Each peer connection has a local forwarder process.
--   The forwarder process has a salted key so it can't be
--   guessed by other nodes. When it's attached connection goes down,
--   it gets killed. Other processes can listen for it's demise to 
--   perform cleanups.
getForwarder :: MonadIO m => Node -> NodeId -> m (ProcessId, Proc)
getForwarder node@Node{..} nodeId = do
  let pid = getForwarderId salt nodeId

  -- TODO: this needs to be atomic. withAtomicIO :: Node -> ...
  mproc <- atomically $ getProcessById node pid
  proc <- maybe (nodeSpawnNamed node pid run) pure mproc

  pure (pid, proc)

  where
  run = do
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
  ProcessId $ blake2b $ salt <> BS8.pack (show nodeId)


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




sendRemote :: (Binary a) => NodeId -> ProcessId -> a -> Process ()
sendRemote nodeId theirPid msg = do
  node <- asks node
  (_, proc) <- getForwarder node nodeId
  atomically $
    -- TODO: RemoteMessage??
    sendProcSTM proc $ SendToPeer theirPid $ encode msg


receiveMaybe :: Typeable a => Process (Maybe a)
receiveMaybe = receiveTimeout 0


receiveWait :: Typeable a => Process a
receiveWait = do
  r <- asks inbox >>= atomically . readTQueue
  case fromDynamic r of
    Nothing -> receiveWait
    Just a -> pure a


-- TODO: Does this function peg CPU?
receiveTimeout :: Typeable a => Int -> Process (Maybe a)
receiveTimeout us = do
  PD{..} <- ask
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

receiveTimeoutS :: Typeable a => Int -> Process (Maybe a)
receiveTimeoutS s = receiveTimeout (s * 1000000)




-- | Network Handler

networkHandler :: Process ()
networkHandler = do
  node@Node{..} <- asks node

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



withRemoteMessage :: Binary i => (NodeId -> i -> Process ()) -> RemoteMessage -> Process ()
withRemoteMessage act (RemoteMessage nodeId bs) = do
  case decodeOrFail bs of
    Right ("", _, a) -> act nodeId a
    Right (_, _, _) -> pure () -- TODO: log
    Left _ -> pure () -- TODO: log

    



-- | Utils -----------


timeDelta :: MonadIO m => UTCTime -> m Int
timeDelta t = f <$> liftIO getCurrentTime where
  f now = round . (*1000000) . realToFrac $ diffUTCTime now t


blake2b :: BS.ByteString -> BS.ByteString
blake2b b = BS.pack (BA.unpack (hash b :: Digest Blake2b_256))


serviceId :: BS.ByteString -> ProcessId
serviceId = ProcessId . blake2b

remoteServiceId :: NodeId -> BS.ByteString -> RemoteProcessId
remoteServiceId nodeId = RemoteProcessId nodeId . serviceId


fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
