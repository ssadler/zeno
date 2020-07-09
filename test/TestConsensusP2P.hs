{-# LANGUAGE OverloadedLists #-}

module TestConsensusP2P where

import TestUtils
import TestNode

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import UnliftIO
import Zeno.Console
import Zeno.Consensus.P2P
import Zeno.Prelude
import Zeno.Process


instance LiftP2P TestBase where
  liftP2P = undefined


test_p2p :: TestTree
test_p2p = testGroup "p2p tests"
  [
    testCase "simple discover" do
      void $ runTestNode 2 do
        flip runStateT ([("0:0", emptyPeerState), ("1:1", emptyPeerState)]) do
          
          node "0:0" do
            newPeer "1:1"
          node "1:1" do
            getMsg >>= withRemoteMessage onPeerMessage
          node "0:0" do
            getMsg >>= withRemoteMessage onPeerMessage
            getMsg >>= withRemoteMessage onPeerMessage
          node "1:1" do
            getMsg >>= withRemoteMessage onPeerMessage

          lift (use _2) >>= (@?= mempty)
          states <- Map.toList . fmap (Set.toList . p2pPeers) <$> get
          states @?= [("0:0", ["1:1"]), ("1:1", ["0:0"])]

  , testCase "doesnt accept localhost from non localhost node" do
      void $ runTestNode 2 do
        flip runStateT ([("0:0", emptyPeerState), ("1:1", emptyPeerState)]) do
          
          node "0:0" do
            onPeerMessage "1:1" $ Peers $ ["127.0.0.1:1"]

        use _2 >>= (@?= mempty)

  , testCase "disconnected peer gets blocked" do
      void $ runTestNode 2 do
        flip runStateT ([("0:0", emptyPeerState), ("1:1", emptyPeerState)]) do
          node "0:0" do
            dropPeer "1:1"
            use _disconnects >>= (@?= ["1"])

          node "1:1" do
            lift $ sendRemote "0:0" p2pCapId GetPeers

          -- ignores it entirely
          node "0:0" do
            getMsg >>= withRemoteMessage onPeerMessage
            lift $ use _2 >>= (@?= mempty)

  , testCase "doesn't try to connect to disconnected peers" do
      void $ runTestNode 2 do
        flip runStateT (Map.fromList [("0:0", emptyPeerState), ("1:1", emptyPeerState)]) do
          node "0:0" do
            dropPeer "2:2"
            onPeerMessage "1:1" $ Peers $ ["2:2", "3:3"]

        use _2 >>= (@?= [("3:3", [RemoteMessage "0:0" "\0"])])
  ]
