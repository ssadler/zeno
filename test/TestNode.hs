
module TestNode where

import TestUtils

import Zeno.Process
import Zeno.Prelude



testLog = FilteredLog LevelWarn PlainLog


withTestNode :: Zeno Node r -> IO r
withTestNode act = do
  runZeno testLog () do
    withNode (NetworkConfig "127.0.0.1" 0) act


test_node_messaging :: TestTree
test_node_messaging = testGroup "node messaging"
  [
    testCase "sends and receives messages in single node" do
      withTestNode do
        myNid <- asks myNodeId
        mbox <- subscribe "a"
        forM_ [0..10] \_ -> do
          sendRemote myNid "a" EQ
          RemoteMessage thatNid EQ <- receiveWait mbox
          liftIO $ thatNid @?= myNid
  ]
