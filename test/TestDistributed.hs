
module TestDistributed where

import TestUtils

import Control.Monad.Reader
import qualified Data.ByteString as BS

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
      pure ()

  , testCase "spawnChild" do
      node <- createTransport >>= startNode
      mblock <- newEmptyMVar
      let block = takeMVar mblock
      handoffChild <- newEmptyMVar
      parent <- nodeSpawn' node $ runReaderT do
        spawnChild (block >> block) >>= putMVar handoffChild
        block

      child <- takeMVar handoffChild
      putMVar mblock ()
      killProcess node $ procId parent
      Left e <- waitCatch (procAsync child)
      show e @?= "AsyncCancelled"



      
  ]

