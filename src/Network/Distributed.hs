{-# LANGUAGE ConstraintKinds #-}

module Network.Distributed 
  ( Node
  , NodeId(..)
  , Process
  , ProcessId
  , Sendable
  , RemoteProcessId(..)
  , createNode
  , getSelfPid
  , match
  , nodeSpawn
  , nodeSpawnNamed
  , nsend
  , remoteServiceId
  , repeatMatch
  , send
  , sendRemote
  , serviceId
  , spawn
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
import Control.Concurrent.STM (check)

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

data Proc = Proc
  { procChan :: TQueue Dynamic
  , procAsync :: Async ()
  } 


-- https://github.com/nikita-volkov/stm-containers
data InnerNode = Inner
  { _lastPid :: BS.ByteString
  , _processes :: Map.Map ProcessId Proc
  , _subscribers :: Map.Map String (Set.Set ProcessId)
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


newtype Process a = Process { unProcess :: ReaderT ProcData IO a }
  deriving ( Functor, Monad, Applicative
           , MonadIO, MonadThrow, MonadCatch
           , MonadReader ProcData, MonadUnliftIO
           )


newtype ProcessId = ProcessId BS.ByteString
  deriving (Show, Eq, Ord, Binary)

data RemoteProcessId = RPID
  { remoteNid :: NodeId
  , remotePid :: ProcessId
  } deriving (Show, Eq, Ord, Generic)

instance Binary RemoteProcessId

type Sendable a = Typeable a


makeLenses ''InnerNode


createNode :: NT.Transport -> IO Node
createNode transport = do
  endpoint <- NT.newEndPoint transport <&> either (error . show) id
  salt <- getEntropy 32
  let _lastPid = ""
      _subscribers = mempty
      _processes = mempty
  tInnerNode <- newTVarIO Inner{..}
  pure Node{..}


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



getSelfPid :: Process ProcessId
getSelfPid = asks myPid

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

nodeSpawnNamed :: MonadIO m => Node -> ProcessId -> Process () -> m ()
nodeSpawnNamed node pid act = do
  chan <- newTQueueIO

  t <- atomically $ newEmptyTMVar
  a <- liftIO $ async do
    r <- PD node pid chan <$> atomically (takeTMVar t)
    runReaderT (unProcess act) r

  atomically do
    modifyInnerNode_ node $ processes . at pid .~ Just (Proc chan a)
    putTMVar t a



spawn :: Process () -> Process ProcessId
spawn p = do
  asks node >>= flip nodeSpawn p


repeatMatch :: Int -> [Match ()] -> Process ()
repeatMatch timeout matches = do
  startTime <- liftIO $ getCurrentTime
  fix $ \f -> do
    d <- liftIO $ timeDelta startTime
    let us = timeout - d
    when (us > 0) $
       receiveTimeout us matches >>= maybe (pure ()) (const f)

-- Spawns a process and links it to it's parent so that
-- it will die when it's parent dies
spawnChild :: Process () -> Process ProcessId
spawnChild proc = do
  parent <- asks myAsync
  spawn do
    child <- asks myAsync
    pure () <* forkIO do
      waitCatch parent >>= \_ -> uninterruptibleCancel child
    proc


closeNode :: Node -> IO ()
closeNode = undefined

whereis :: String -> Process (Maybe a)
whereis = undefined

register :: String -> ProcessId -> Process ()
register s p = do
  node <- asks node
  atomically do
    modifyInnerNode_ node $
      subscribers %~ (<> Map.singleton s (Set.singleton p))




type Match a = Dynamic -> Maybe (Process a)

match :: Sendable a => (a -> Process b) -> Match b
match f dyn = f <$> fromDynamic dyn


say :: String -> Process ()
say = error "say"

send :: Sendable a => ProcessId -> a -> Process ()
send pid msg = do
  node <- asks node
  atomically $ sendSTM node pid msg


sendSTM :: Sendable a => Node -> ProcessId -> a -> STM ()
sendSTM node pid msg = do
  getProcessById node pid >>=
    \case
      Nothing -> pure ()
      Just proc -> sendProcSTM proc msg


sendProcSTM :: Sendable a => Proc -> a -> STM ()
sendProcSTM Proc{..} = writeTQueue procChan . toDyn

type Topic = String


nsend :: Sendable a => Topic -> a -> Process ()
nsend topic msg = do
  node <- asks node
  atomically do
    Inner{..} <- readInnerNode node
    let recips = Map.findWithDefault mempty topic _subscribers :: Set.Set ProcessId
    forM_ recips $ \pid -> sendSTM node pid msg


data RemoteCommand = PeerMsg ProcessId PeerMsg
  deriving (Typeable)


data PeerMsg = SendMsg BSL.ByteString
  deriving (Typeable, Generic)

instance Binary PeerMsg


getRemoteConnection :: MonadIO m => Node -> NodeId -> m ProcessId
getRemoteConnection node@Node{..} nodeId = do
  let pid = ProcessId $ blake2b $ salt <> BS8.pack (show nodeId)
  mproc <- atomically $ getProcessById node pid
  case mproc of
    Nothing -> do
      nodeSpawnNamed node pid run
    Just _ -> pure ()
  pure pid

  where
  run = do
    liftIO (setupConn endpoint nodeId) >>= 
      \case Nothing   -> pure ()                -- TODO: log?
            Just conn -> loopHandle conn

  loopHandle conn = do
    receiveWait [match pure] >>= 
      \case
        PeerMsg topic msg -> do
          _ <- liftIO $ NT.send conn (BSL.toChunks $ encode msg) -- TODO: failure?
          loopHandle conn



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






sendRemote :: (Sendable a, Binary a) => RemoteProcessId -> a -> Process ()
sendRemote (RPID nodeId theirPid) msg = do
  node <- asks node
  pid <- getRemoteConnection node nodeId
  send pid $ PeerMsg theirPid $ SendMsg $ encode msg



receiveWait :: Sendable a => [Match a] -> Process a
receiveWait matchers = do
  inbox <- asks inbox
  msg <- atomically $ readTQueue inbox
  let matched = matchMessage matchers msg
  maybe (receiveWait matchers) id matched

matchMessage :: Sendable a => [Match a] -> Dynamic -> Maybe (Process a)
matchMessage matchers msg =
  listToMaybe $ catMaybes [f msg | f <- matchers]


-- TODO: Does this function peg CPU?
receiveTimeout :: Typeable a => Int -> [Match a] -> Process (Maybe a)
receiveTimeout us matchers = do
  PD{..} <- ask
  delay <- registerDelay us
  join $
    atomically do
      md <- tryReadTQueue inbox
      case md >>= matchMessage matchers of
        Nothing -> (readTVar delay >>= check) *> pure (pure Nothing)
        Just ma -> pure $ Just <$> ma


    


--

processNodeId :: ProcessId -> NodeId
processNodeId = undefined

monitor :: ProcessId -> Process ()
monitor = undefined

unmonitor :: String -> Process ()
unmonitor = undefined

expectTimeout :: Sendable a => Integer -> Process (Maybe a)
expectTimeout = undefined

releaseNode :: Node -> IO ()
releaseNode = undefined



-- | Utils -----------


timeDelta :: UTCTime -> IO Int
timeDelta t = us <$> (diffUTCTime <$> getCurrentTime <*> pure t)
  where us = round . (*1000000) . realToFrac


blake2b :: BS.ByteString -> BS.ByteString
blake2b b = BS.pack (BA.unpack (hash b :: Digest Blake2b_256))


serviceId :: BS.ByteString -> ProcessId
serviceId = ProcessId . blake2b

remoteServiceId :: NodeId -> BS.ByteString -> RemoteProcessId
remoteServiceId nodeId = RPID nodeId . serviceId
