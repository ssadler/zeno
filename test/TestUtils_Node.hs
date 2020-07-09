
module TestUtils_Node where


import Test.Tasty.HUnit

import           Control.Monad.State
import qualified Data.Map as Map
import           Zeno.Process
import           Zeno.Prelude
import           Zeno.Consensus.P2P
import           Zeno.Console


newtype TestIO a = TestIO { runTestIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
instance MonadLogger TestIO where monadLoggerLog a b c d = pure ()

testIOCase s act = testCase s $ runTestIO act


type TestNodeState =
  ( [NodeId]                                   -- peers
  , Map NodeId [RemoteMessage LazyByteString]  -- mailboxes
  , Maybe NodeId                               -- currently focused node
  )
_mboxes :: Lens' TestNodeState [NodeId]
_mboxes = _1
emptyTestNode = (mempty, mempty, Nothing) :: TestNodeState
runTestNode :: Int -> StateT TestNodeState TestIO a -> IO (a, TestNodeState)
runTestNode npeers act = runTestIO $ runStateT act (testNodeId <$> [0..npeers-1], mempty, Nothing)

testNodeId :: Int -> NodeId
testNodeId i = NodeId (show i) (fromIntegral i)



instance HasNode TestBase where
  type HandlerMonad TestBase = TestBase
  registerCapability cap h = undefined

  sendRemoteBS to _cap lbs = do
    Just i <- use _3
    let from = i
    _2 %= Map.insertWith (flip (++)) to [RemoteMessage from lbs]

  monitorRemote _ _ = pure ()
  getMyIp = maybe undefined show <$> use _3

instance HasP2P TestBase where
  getPeers = use _1


type TestBase = StateT TestNodeState TestIO

instance MonadLoggerUI TestBase where sendUI _ = pure ()


node :: NodeId -> StateT s TestBase a -> StateT (Map.Map NodeId s) TestBase a
node n act = do
  s <- get
  join $ lift do
    _3 .= Just n
    peers <- _1 <<%= filter (/=n)
    (a, s') <- runStateT act (s Map.! n)
    _3 .= Nothing
    _1 .= peers
    pure do
      put $ set (ix n) s' s
      pure a


getMsgMaybe :: StateT a TestBase (Maybe (RemoteMessage LazyByteString))
getMsgMaybe = do
  Just to <- lift $ use _3
  lift do
    zoom _2 do
      state \m ->
        case Map.lookup to m of
          Just [o]    -> (Just o, Map.delete to m)
          Just (o:xs) -> (Just o, Map.insert to xs m)
          _           -> (Nothing, m)

getMsg :: StateT a TestBase (RemoteMessage LazyByteString)
getMsg = do
  Just i <- lift $ use _3
  getMsgMaybe >>= maybe (error $ "no items for " ++ show i) pure

newNodeStates :: Int -> a -> Map.Map NodeId a
newNodeStates n a = Map.fromList $ zip (testNodeId <$> [0..n-1]) (repeat a)
