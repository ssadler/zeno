{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Zeno.Monad where

import Control.Exception.Safe (MonadMask)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource as ResourceT
import UnliftIO

import Zeno.Logging

import Debug.Trace



newtype Zeno r a =
  Zeno { unZeno :: forall ret. (r -> RTI -> a -> IO ret) -> r -> RTI -> IO ret }

type RTI = ResourceT.InternalState

instance Functor (Zeno r) where
  fmap f (Zeno p) = Zeno $
    \rest -> p (\r rti -> rest r rti . f)

instance Applicative (Zeno r) where
  pure = return
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Zeno r) where
  return x = Zeno $ \f r rti -> f r rti x
  {-# INLINE return #-}
  Zeno f >>= g = Zeno $
    \rest -> f (\r rti a -> unZeno (g a) rest r rti)
  {-# INLINE (>>=) #-}

instance Semigroup a => Semigroup (Zeno r a) where
  ma <> mb = (<>) <$> ma <*> mb

instance Monoid a => Monoid (Zeno r a) where
  mempty = pure mempty

instance MonadIO (Zeno r) where
  liftIO io = Zeno $ \f r rti -> io >>= f r rti

instance MonadReader r (Zeno r) where
  ask = Zeno $ \f r rti -> f r rti r
  {-# INLINE ask #-}
  local f (Zeno p) = Zeno $ \rest -> p (\r rti -> rest (f r) rti)

instance MonadResource (Zeno r) where
  liftResourceT resT = Zeno $
    \f r rti -> runInternalState resT rti >>= f r rti

instance MonadLogger (Zeno r) where
  monadLoggerLog a b c d = liftIO $ logStderr a b c (toLogStr d)

instance MonadLoggerIO (Zeno r) where
  askLoggerIO = pure logStderr

instance MonadUnliftIO (Zeno r) where
  withRunInIO inner =
    Zeno $ \f r rti -> inner (\(Zeno z) -> z (\_ _ -> pure) r rti) >>= f r rti


runZeno :: r -> Zeno r a -> IO a
runZeno r (Zeno f) = do
  bracket ResourceT.createInternalState
          ResourceT.closeInternalState
          (f (\_ _ -> pure) r)


-- | Cleanup resources on exit
withLocalResources :: Zeno r a -> Zeno r a
withLocalResources act = do
  r <- ask
  liftIO $ runZeno r act


withZeno :: (r -> r') -> Zeno r' a -> Zeno r a
withZeno f (Zeno ma) = Zeno \rest r rti -> ma (\_ _ -> pure) (f r) rti >>= rest r rti


-- The Has type
class Has r a where
  has :: a -> r

instance Has r r where
  has = id

hasReader :: Has r' r => Zeno r' a -> Zeno r a
hasReader = withZeno has


