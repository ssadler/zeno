
module Zeno.Monad where

import Control.Monad.Reader
import Control.Monad.Trans.Resource as ResourceT
import qualified Data.Set as Set
import Data.Text (Text)
import Lens.Micro.Platform hiding (has)
import UnliftIO
import Zeno.Console.Types

--------------------------------------------------------------------------------
-- | Zeno App context
--------------------------------------------------------------------------------

data ZenoApp r = App
  { _context   :: r
  , _console   :: Console
  , _resources :: ResourceT.InternalState
  } deriving (Functor)

makeLenses ''ZenoApp

--------------------------------------------------------------------------------
-- | Zeno monad and instances
--------------------------------------------------------------------------------

newtype Zeno r a = Zeno { unZeno :: ReaderT (ZenoApp r) IO a }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadUnliftIO)

instance Semigroup a => Semigroup (Zeno r a) where
  ma <> mb = (<>) <$> ma <*> mb

instance Monoid a => Monoid (Zeno r a) where
  mempty = pure mempty

instance MonadReader r (Zeno r) where
  ask = Zeno $ _context <$> ask
  {-# INLINE ask #-}
  local = withContext

instance MonadResource (Zeno r) where
  liftResourceT resT = Zeno do
    resources <- asks _resources
    liftIO $ runInternalState resT resources

--------------------------------------------------------------------------------
-- | Zeno runners
--------------------------------------------------------------------------------

runZeno :: Console -> r -> Zeno r a -> IO a
runZeno console r act = runReaderT (unZeno (withLocalResources act)) app
  where app = App r console undefined

localZeno :: (ZenoApp r -> ZenoApp r') -> Zeno r' a -> Zeno r a
localZeno f (Zeno z) = Zeno $ ask >>= liftIO . runReaderT z . f
{-# INLINE localZeno #-}

withLocalResources :: Zeno r a -> Zeno r a
withLocalResources z = do
  bracket
    ResourceT.createInternalState
    ResourceT.closeInternalState
    (\rti -> localZeno (resources .~ rti) z)

withContext :: (r -> r') -> Zeno r' a -> Zeno r a
withContext = localZeno . fmap

getConsole :: Zeno r Console
getConsole = Zeno $ asks _console

withHideTrace :: Text -> Zeno r a -> Zeno r a
withHideTrace lvl = localZeno $ console . logDebugMask %~ Set.delete lvl

--------------------------------------------------------------------------------
-- | Has typeclass
--------------------------------------------------------------------------------

class Has r a where
  has :: a -> r

instance Has r r      where has = id
instance Has a (a, b) where has = fst
instance Has b (a, b) where has = snd

hasReader :: Has r' r => Zeno r' a -> Zeno r a
hasReader = withContext has

