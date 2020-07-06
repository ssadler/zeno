
module TestNode where

import TestUtils

import Zeno.Process
import Zeno.Prelude

import UnliftIO


testLog = consoleWarn


withTestNode :: Zeno Node r -> IO r
withTestNode act = do
  runZeno testLog () do
    withNode (NetworkConfig "127.0.0.1" 0 (pure "127.0.0.1")) act

withTestNodes :: Int -> Zeno [Node] r -> IO r
withTestNodes n act = go n []
  where
  go 0 nodes = runZeno testLog nodes act
  go n nodes = withTestNode do node <- ask; liftIO $ go (n-1) (node:nodes)


test_node_messaging :: TestTree
test_node_messaging = testGroup "node messaging"
  [
    testCase "sends and receives messages in single node" do
      withTestNode do
        myNid <- asks myNodeId
        mbox <- newEmptyMVar
        registerCapability 10 $ putMVar mbox
        forM_ [0..10] \_ -> do
          sendRemote myNid 10 (2 :: Word8)
          RemoteMessage thatNid msg <- takeMVar mbox
          msg @?= "\2"
          liftIO $ thatNid @?= myNid
  ]
