
module TestInboundRateLimit where

import TestUtils
import Test.DejaFu
import Test.Tasty.DejaFu
import Zeno.Process.Node.InboundRateLimit


test_limitInboundConnections =
  testGroup "limitInboundConnections"
  [
    testAuto testInboundConnectionLimit
  ]
