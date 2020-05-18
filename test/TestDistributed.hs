
module TestDistributed where

import TestUtils

import qualified Data.ByteString as BS
import Control.Monad.Reader

import Network.Distributed
import Network.Transport.InMemory


test_distributed = testGroup "distributed" $
  [
    testCase "startNode" do
      transport <- createTransport
      Node{..} <- startNode transport
      BS.length salt @?= 32
      pure ()

  ]

