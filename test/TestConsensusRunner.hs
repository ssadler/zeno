{-# LANGUAGE MultiWayIf #-}

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
  [step0, step1] <- testSteps 0 idents2

  void $ runTestNode 2 do
    flip runStateT (replicate 2 emptyRunnerState) do
      node 0 do
        handleEvent $ NewStep minBound $ createStep step0 $ Just 0
      node 1 do
        handleEvent $ NewStep minBound $ createStep step1 $ Just 1

      node 0 $ getMsg >>= handleEvent . PeerMessage
      node 1 $ getMsg >>= handleEvent . PeerMessage
      
      dumpInv step0 >>= (@?= targetInv0)
      dumpInv step1 >>= (@?= targetInv0)

    msgMap <- use _2 <&> over (each . each) (decodeAuthenticated step0 . fmap (BSL.drop 12))
    msgMap @?=
      Map.fromList [ ("0:0", [Right (StepMessage 3 0 mempty)])
                   , ("1:1", [Right (StepMessage 3 0 mempty)])
                   ]


unit_test_miss_cache :: IO ()
unit_test_miss_cache = do
  [step0, step1] <- testSteps 0 idents2

  void $ runTestNode 2 do
    flip runStateT (replicate 2 emptyRunnerState) do

      node 0 $ handleEvent $ NewStep minBound $ createStep step0 $ Just 0
      node 1 do
        getMsg >>= handleEvent . PeerMessage
        use (_missCache . to length) >>= (@?= 1)
        handleEvent $ NewStep minBound $ createStep step1 $ Just 1
        use (_missCache . to length) >>= (@?= 0)
      dumpInv step1 >>= (@?= targetInv0)


data RoundState
  = INIT
  | STEP Int (Step Int) (Inventory Int)
  | DONE
  deriving (Eq)

instance Eq (Step i) where
  s == s1 = stepId s == stepId s1

instance Show RoundState where
  show INIT = "INIT"
  show DONE = "DONE"
  show (STEP sid Step{..} inv) = "STEP %i %i" % (sid, length inv)



unit_test_round_ideal :: IO ()
unit_test_round_ideal = do

  let nnodes = 3
  let nsteps = 3
  allSteps <- forM [0..nsteps-1] \i -> testSteps i (take nnodes identsInf)

  void $ runTestNode nnodes do
    flip runStateT (replicate nnodes emptyRunnerState) do

      -- WHILE loop
      fix1 (replicate nnodes INIT) \go r -> do

        -- for each node
        r' <- forM (zip [0..] r) \(n, s) -> do
          node n do
            case s of
              DONE -> pure DONE
              INIT -> do
                let step = allSteps !! 0 !! n
                handleEvent $ NewStep minBound $ createStep step $ Just n
                inv <- snd <$> readIORef (ioInv step)
                pure $ STEP 0 step inv

              STEP stepNum stepData inv -> do
                getMsgMaybe >>= mapM_ (handleEvent . PeerMessage)
                inv <- snd <$> readIORef (ioInv stepData)
                if | length inv < nnodes -> pure $ STEP stepNum stepData inv
                   | stepNum == nsteps-1 -> pure DONE
                   | otherwise -> do
                       let step = allSteps !! (stepNum+1) !! n
                       handleEvent $ NewStep minBound $ createStep step $ Just n
                       inv <- snd <$> readIORef (ioInv step)
                       pure $ STEP (stepNum+1) step inv

        mboxes <- lift $ use _mboxes
        if | (r == r' && length mboxes == 0) -> fail $ "blocked: " ++ show r'
           | r /= replicate nnodes DONE -> go r'
           | otherwise -> pure ()






identsInf = map deriveEthIdent $ drop 1 [minBound..]
idents2 = take 2 identsInf

targetInv0 = (3, inv) where
  inv = [ ("0x897df33a7b3c62ade01e22c13d48f98124b4480f", 1)
        , ("0xdc5b20847f43d67928f49cd4f85d696b5a7617b5", 0)
        ]


-- | Get inventory without signatures
dumpInv :: MonadIO m => Step i -> m (PackedInteger, [(Address, i)])
dumpInv step = do
  (mask, invMap) <- readIORef $ ioInv step
  pure $ (mask, over (each . _2) snd $ Map.toList invMap)


testSteps :: MonadIO m => Int -> [EthIdent] -> m [Step Int]
testSteps stepNum idents = do
  let members = ethAddress <$> idents
      membersSet = Set.fromList members
      stepId = StepId minBound $ fromIntegral stepNum
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
_mboxes = _1
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

getMsgMaybe :: StateT a TestBase (Maybe (RemoteMessage LazyByteString))
getMsgMaybe = do
  Just i <- lift $ use _3
  let to = testNodeId i
  lift do
    zoom _2 do
      state \m ->
        case Map.lookup to m of
          Just (o:xs) -> (Just o, Map.insert to xs m)
          _           -> (Nothing, m)

getMsg :: StateT a TestBase (RemoteMessage LazyByteString)
getMsg = do
  Just i <- lift $ use _3
  getMsgMaybe >>= maybe (error $ "no items for " ++ show i) pure


instance HasNode TestBase where
  type HandlerMonad TestBase = TestBase
  registerCapability cap h = undefined

  sendRemoteBS to _cap lbs = do
    Just i <- use _3
    let from = testNodeId i
    _2 %= Map.insertWith (flip (++)) to [RemoteMessage from lbs]

instance HasP2P TestBase where
  getPeers = use _1
