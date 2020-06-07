{-# LANGUAGE RankNTypes #-}

module Zeno.Monad where

import Control.Monad.Catch as Catch hiding (bracket)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource as ResourceT

import Lens.Micro.Platform hiding (has)

import UnliftIO

import Zeno.Logging
import GHC.Stack

--------------------------------------------------------------------------------
-- | Zeno App context
--------------------------------------------------------------------------------

data ZenoApp r = App
  { _context   :: r
  , _console   :: Console
  , _resources :: ResourceT.InternalState
  }

makeLenses ''ZenoApp

instance Functor ZenoApp where
  fmap f z = z & context %~ f

--------------------------------------------------------------------------------
-- | Zeno monad and instances
--------------------------------------------------------------------------------

newtype Zeno r a =
  Zeno { unZeno :: forall ret. (ZenoApp r -> a -> IO ret) -> ZenoApp r -> IO ret }

instance Functor (Zeno r) where
  fmap f (Zeno p) = Zeno $
    \rest -> p (\app -> rest app . f)
  {-# INLINE fmap #-}

instance Applicative (Zeno r) where
  pure x = Zeno $ \f app -> f app x
  {-# INLINE pure #-}
  (<*>) = ap

instance Monad (Zeno r) where
  return = pure
  Zeno f >>= g = Zeno $
    \rest -> f (\app a -> unZeno (g a) rest app)
  {-# INLINE (>>=) #-}

instance Semigroup a => Semigroup (Zeno r a) where
  ma <> mb = (<>) <$> ma <*> mb

instance Monoid a => Monoid (Zeno r a) where
  mempty = pure mempty

instance MonadIO (Zeno r) where
  liftIO io = Zeno $ \f app -> io >>= f app
  {-# INLINE liftIO #-}

instance MonadUnliftIO (Zeno r) where
  withRunInIO inner = Zeno
    \f app -> inner (\(Zeno z) -> z (\_ -> pure) app) >>= f app

instance MonadReader r (Zeno r) where
  ask = Zeno $ \f app -> f app $ _context app
  {-# INLINE ask #-}
  local = withContext

instance MonadResource (Zeno r) where
  liftResourceT resT = Zeno
    \f app -> runInternalState resT (_resources app) >>= f app

instance MonadLogger (Zeno r) where
  monadLoggerLog a b c d = Zeno
    \rest app -> logMessage (_console app) a b c d >>= rest app

instance MonadLoggerIO (Zeno r) where
  askLoggerIO = Zeno
    \rest app -> rest app (logMessage (_console app))

instance MonadFail (Zeno r) where
  fail = error

instance MonadCatch (Zeno r) where
  catch (Zeno z) onErr = Zeno
    \f app -> Catch.catch (z f app) (\e -> unZeno (onErr e) f app)

instance MonadMask (Zeno r) where
  mask withUnmask = Zeno
    \f app ->
      Catch.mask \unmask ->
        unZeno (withUnmask $ \z -> Zeno \f' app' -> unmask $ unZeno z f' app') f app

  uninterruptibleMask withUnmask = Zeno
    \f app ->
      Catch.uninterruptibleMask \unmask ->
        unZeno (withUnmask $ \z -> Zeno \f' app' -> unmask $ unZeno z f' app') f app

  generalBracket allocate' release' inner = Zeno
    \f app -> do
      f app =<<
           generalBracket (         unZeno allocate'       (\_ -> pure) app)
                          (\a ec -> unZeno (release' a ec) (\_ -> pure) app)
                          (\a    -> unZeno (inner a)       (\_ -> pure) app)

instance MonadThrow (Zeno r) where
  throwM e = Zeno \_ _ -> throwM e


--------------------------------------------------------------------------------
-- | Zeno runners
--------------------------------------------------------------------------------

runZeno :: Console -> r -> Zeno r a -> IO a
runZeno console r act = unZeno (withLocalResources act) (\_ -> pure) app 
  where app = App r console undefined

localZeno :: (ZenoApp r -> ZenoApp r') -> Zeno r' a -> Zeno r a
localZeno f (Zeno z) = Zeno \rest app -> z (\_ -> rest app) (f app)
{-# INLINE localZeno #-}

withLocalResources :: Zeno r a -> Zeno r a
withLocalResources z = do
  bracket ResourceT.createInternalState
          ResourceT.closeInternalState
          (\rti -> localZeno (resources .~ rti) z)

withContext :: (r -> r') -> Zeno r' a -> Zeno r a
withContext = localZeno . fmap

getConsole :: Zeno r Console
getConsole = Zeno \rest app -> rest app (_console app)

--------------------------------------------------------------------------------
-- | Has typeclass
--------------------------------------------------------------------------------

class Has r a where
  has :: HasCallStack => a -> r

instance Has r r where
  has = id

hasReader :: Has r' r => Zeno r' a -> Zeno r a
hasReader = withContext has

