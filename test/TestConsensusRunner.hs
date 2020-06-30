
module TestConsensusRunner where

import TestUtils

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.ByteString.Lazy as BSL
import Data.List (uncons)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Network.Ethereum.Crypto

import UnliftIO

import Zeno.Process
import Zeno.Consensus
import Zeno.Consensus.P2P
import Zeno.Consensus.Runner
import Zeno.Consensus.Step
import Zeno.Prelude


unit_test_sync :: IO ()
unit_test_sync = do

  idents <- mapM deriveEthIdent [ "dc1a9f817d7c865db1ee50473b67db147b13f0b2b8ae12e4c876c74cc7285d5c"
                                , "12d338400e8e5a91375e29c65c8bbff73f48d47bd24abf21bea8dc49c6a18531"
                                ]

  let targetInv = (3, inv)
        where inv = [ ("0x970c57f44720e0ab7730132b03ab641475a6c102", 0)
                    , ("0xb49bc7852acbff6040787a3f4b735508215555e5", 1)
                    ]

  [step0, step1] <- testSteps idents

  runTestNode 2 do
    flip runStateT (replicate 2 emptyRunnerState) do
      node 0 do
        handleEvent $ NewStep minBound $ createStep step0 $ Just 0
      node 1 do
        handleEvent $ NewStep minBound $ createStep step1 $ Just 1

      node 0 $ getMsg >>= handleEvent . PeerMessage
      node 1 $ getMsg >>= handleEvent . PeerMessage
      
      dumpInv step0 >>= (@?= targetInv)
      dumpInv step1 >>= (@?= targetInv)

    msgMap <- use _2 <&> over (each . each) (decodeAuthenticated step0 . fmap (BSL.drop 12))
    msgMap @?=
      Map.fromList [ ("0:0", [Right (StepMessage 3 0 mempty)])
                   , ("1:1", [Right (StepMessage 3 0 mempty)])
                   ]

  pure ()

dumpInv :: MonadIO m => Step i -> m (PackedInteger, [(Address, i)])
dumpInv step = do
  (mask, invMap) <- readIORef $ ioInv step
  pure $ (mask, over (each . _2) snd $ Map.toList invMap)

testSteps :: MonadIO m => [EthIdent] -> m [Step Int]
testSteps idents = do
  let members = ethAddress <$> idents
      membersSet = Set.fromList members
      stepId = StepId minBound 0
      yield inv = pure ()
  forM idents \ident -> do
    ioInv <- newIORef (0, mempty)
    pure Step{..}


type TestBase = StateT TestNodeState TestIO

type TestNodeState =
  ( [NodeId]
  , Map NodeId [RemoteMessage LazyByteString]
  , Maybe Int -- currently focused node
  )
emptyTestNode = (mempty, mempty, Nothing) :: TestNodeState
runTestNode npeers act = runTestIO $ runStateT act (testNodeId <$> [0..npeers-1], mempty, Nothing)

testNodeId :: Int -> NodeId
testNodeId i = NodeId (show i) (fromIntegral i)

node :: Int -> StateT (RunnerState TestBase) TestBase a
            -> StateT [RunnerState TestBase] TestBase a
node n act = do
  s <- get
  join $ lift do
    _3 .= Just n
    peers <- _1 <<%= filter (/=testNodeId n)
    (a, s') <- runStateT act (s !! n)
    _3 .= Nothing
    _1 .= peers
    pure do
      put $ set (ix n) s' s
      pure a

getMsg :: StateT a TestBase (RemoteMessage LazyByteString)
getMsg = do
  Just i <- lift $ use _3
  let to = testNodeId i
  lift do
    zoom _2 do
      state \m ->
        case Map.lookup to m of
          Just (o:xs) -> (o, Map.insert to xs m)
          _           -> error $ "no items for " ++ show i


instance HasNode TestBase where
  type HandlerMonad TestBase = TestBase
  registerCapability cap h = undefined

  sendRemoteBS to _cap lbs = do
    Just i <- use _3
    let from = testNodeId i
    _2 %= Map.insertWith (flip (++)) to [RemoteMessage from lbs]

instance HasP2P TestBase where
  getPeers = use _1
