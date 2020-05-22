{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Zeno.Monad where

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader

import           Network.Distributed.Types

import           UnliftIO

import Control.Exception.Safe (MonadMask)


newtype Zeno r a = Zeno { unZeno :: ReaderT r (LoggingT IO) a }
  deriving (MonadReader r, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO)

instance Functor (Zeno r) where
  fmap f (Zeno a) = Zeno $ fmap f a

instance Applicative (Zeno r) where
  pure a = Zeno $ pure a
  (Zeno f) <*> (Zeno a) = Zeno $ f <*> a

instance Monad (Zeno r) where
  (Zeno a) >>= f = Zeno $ a >>= unZeno . f

instance MonadIO (Zeno r) where
  liftIO a = Zeno $ liftIO a

instance MonadLogger (Zeno r) where
  monadLoggerLog a b c d = Zeno $ monadLoggerLog a b c d

runZeno :: r -> Zeno r a -> IO a
runZeno r (Zeno act) = runStderrLoggingT $ runReaderT act r

zenoReader :: (r -> r') -> Zeno r' a -> Zeno r a
zenoReader f = Zeno . withReaderT f . unZeno

-- The Has type
class Has r a where
  has :: a -> r

instance Has r r where
  has = id

hasReader :: Has r' r => Zeno r' a -> Zeno r a
hasReader = zenoReader has

-- Process

newtype ZenoProcess r i (m :: * -> *) a = ZenoProcess (Zeno (r i) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (r i), MonadLogger)

instance MonadUnliftIO (ZenoProcess r i m)

instance Typeable i => MonadProcess i IO (ZenoProcess ProcessData) where
  procAsk = ask
  procLift = liftIO
  procRun = runZenoProcess

runZenoProcess (ZenoProcess act) pd = runZeno pd act

instance (MonadProcess i IO (ZenoProcess r), MonadProcess i2 IO (ZenoProcess r))
         => ForkProcess (ZenoProcess r) i i2 IO where


