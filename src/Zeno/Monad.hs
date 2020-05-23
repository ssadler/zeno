{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Zeno.Monad where

import           Control.Exception.Safe (MonadMask)
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Control.Monad.Reader
import           System.IO.Unsafe     -- A good start
import           UnliftIO


type Zeno r = ZenoT r IO

newtype ZenoT r m a = Zeno { unZeno :: ReaderT r m a }
  deriving ( Functor, Applicative, Monad
           , MonadIO
           , MonadReader r
           , MonadThrow, MonadCatch, MonadMask)

instance MonadIO m => MonadLogger (ZenoT r m) where
  monadLoggerLog a b c d = liftIO $ logStdErr a b c (toLogStr d)

instance MonadIO m => MonadLoggerIO (ZenoT r m) where
  askLoggerIO = pure logStdErr

-- A hack gives us the logging function
logStdErr = unsafePerformIO $ runStderrLoggingT $ LoggingT pure

runZeno :: r -> ZenoT r m a -> m a
runZeno r (Zeno act) = runReaderT act r

zenoReader :: (r -> r') -> ZenoT r' m a -> ZenoT r m a
zenoReader f = Zeno . withReaderT f . unZeno

-- The Has type
class Has r a where
  has :: a -> r

instance Has r r where
  has = id

hasReader :: Has r' r => ZenoT r' m a -> ZenoT r m a
hasReader = zenoReader has
