
module TestDistributed where

import TestUtils

import Control.Monad.Reader
import qualified Data.ByteString as BS

import qualified StmContainers.Map as STM

import Network.Distributed
import Network.Transport.InMemory
import UnliftIO


test_distributed = testGroup "distributed" $
  [
    testCase "startNode" do
      Node{..} <- createTransport >>= startNode
      BS.length salt @?= 32
      salt == BS.replicate 0 32 @?= False
      pure ()

  , testCase "spawn" do
      node <- createTransport >>= startNode
      handoff <- newEmptyMVar
      handle <- nodeSpawn' node $ runReaderT $ getMyPid >>= putMVar handoff
      takeMVar handoff >>= (@?= procId handle)
      Right () <- waitCatch (procAsync handle)
      Nothing <- atomically $ getProcessById node $ procId handle

      assertNProcs node 1

  , testCase "spawnChild: terminate parent terminates child" do
      node <- createTransport >>= startNode
      syncChild <- newEmptyMVar
      handoffChild <- newEmptyMVar
      parent <- nodeSpawn' node $ runReaderT do
        child <- spawnChild do
          putMVar syncChild ()
          putMVar syncChild ()               -- will block
        putMVar handoffChild child
        putMVar handoffChild child           -- will block

      readMVar syncChild
      child <- readMVar handoffChild
      assertNProcs node 3

      killProcess node $ procId parent
      assertCancelled parent
      assertCancelled child
      assertNProcs node 1

  , testCase "spawnChild: parent returns normally terminates child" do
      node <- createTransport >>= startNode
      syncChild <- newEmptyMVar
      handoffChild <- newEmptyMVar
      parent <- nodeSpawn' node $ runReaderT do
        child <- spawnChild do
          putMVar syncChild ()
          putMVar syncChild ()               -- will block
        putMVar handoffChild child

      readMVar syncChild
      assertNProcs node 2
      child <- readMVar handoffChild
      Right () <- waitCatch (procAsync parent)

      killProcess node $ procId parent
      assertCancelled child
      assertNProcs node 1
  ]


assertCancelled proc = 
  waitCatch (procAsync proc) >>= (\e -> show e @?= "Left AsyncCancelled")
assertNProcs node n = 
  atomically (STM.size $ processes node) >>= (@?= n)
