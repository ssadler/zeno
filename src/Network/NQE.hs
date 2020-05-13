{-# LANGUAGE ConstraintKinds #-}

module Network.NQE where

import Control.Monad
import Control.Monad.Catch
import Control.Applicative
import Control.Monad.IO.Class

import Data.Binary

import GHC.Generics (Generic)

import Network.Transport

data LocalNode = LocalNode

newtype NodeId = NodeId EndPointAddress
  deriving (Binary)

data WhereIsReply = WhereIsReply String (Maybe ProcessId)
  deriving (Generic)

instance Binary WhereIsReply

data ProcessMonitorNotification = ProcessMonitorNotification String ProcessId String
  deriving (Generic)

instance Binary ProcessMonitorNotification

newtype SendPort a = SendPort ()
  deriving (Binary)

data ReceivePort a = ReceivePort

newtype Process a = Process { unProcess :: IO a }
  deriving (Functor, Monad, Applicative, MonadIO, MonadThrow, MonadCatch)

data Match a = Match

newtype ProcessId = ProcessId Integer
  deriving (Show, Eq, Ord, Binary)

type Sendable = Binary

receiveTimeout :: Int -> [Match a] -> Process (Maybe a)
receiveTimeout = undefined

getSelfPid :: Process ProcessId
getSelfPid = undefined


spawnLocal :: Process () -> Process ProcessId
spawnLocal = undefined


link :: ProcessId -> Process ()
link = undefined

runProcess :: LocalNode -> Process () -> IO ()
runProcess = undefined

newLocalNode :: Transport -> IO LocalNode
newLocalNode = undefined

closeLocalNode :: LocalNode -> IO ()
closeLocalNode = undefined

forkProcess :: LocalNode -> Process () -> IO ProcessId
forkProcess = undefined

whereis :: String -> Process (Maybe a)
whereis = undefined

register :: String -> ProcessId -> Process ()
register s p = undefined

match :: Sendable a => (a -> Process ()) -> Match ()
match = undefined

whereisRemoteAsync :: NodeId -> String -> Process ()
whereisRemoteAsync = undefined

say :: String -> Process ()
say = undefined

send :: Sendable a => ProcessId -> a -> Process ()
send = undefined

nsend :: Sendable a => String -> a -> Process ()
nsend = undefined

nsendRemote :: Sendable a => NodeId -> String -> a -> process ()
nsendRemote = undefined

receiveWait :: [Match ()] -> Process ()
receiveWait = undefined

sendChan :: Sendable a => SendPort a -> a -> Process ()
sendChan = undefined

newChan :: Sendable a => Process (SendPort a, ReceivePort a)
newChan = undefined

receiveChan :: Sendable a => ReceivePort a -> Process a
receiveChan = undefined

receiveChanTimeout :: Sendable a => Int -> ReceivePort a -> Process (Maybe a)
receiveChanTimeout = undefined

processNodeId :: ProcessId -> NodeId
processNodeId = undefined

monitor :: ProcessId -> Process ()
monitor = undefined

unmonitor :: String -> Process ()
unmonitor = undefined


expectTimeout :: Sendable a => Integer -> Process (Maybe a)
expectTimeout = undefined
