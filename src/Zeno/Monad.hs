{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Zeno.Monad where

import Control.Exception.Safe (MonadMask)
import Control.Monad.Catch
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Resource
import System.IO.Unsafe
import UnliftIO


type Zeno r = ZenoT r IO

newtype ZenoT r m a = Zeno { unZeno :: ReaderT r m a }
  deriving ( Functor, Applicative, Monad
           , MonadIO , MonadReader r , MonadTrans
           , MonadThrow, MonadCatch, MonadMask)

instance Semigroup (Zeno r a) where
  a <> b = a *> b

instance Monoid a => Monoid (Zeno r a) where
  mempty = pure mempty

instance MonadUnliftIO m => MonadUnliftIO (ZenoT r m) where
  withRunInIO inner =
    Zeno $ ReaderT $ \r -> withRunInIO $ \rio -> inner (rio . runZeno r)

instance MonadIO m => MonadLogger (ZenoT r m) where
  monadLoggerLog a b c d = liftIO $ logStderr a b c (toLogStr d)

instance MonadIO m => MonadLoggerIO (ZenoT r m) where
  askLoggerIO = pure logStderr

-- A hack gives us the logging function
logStderr = unsafePerformIO $ runStderrLoggingT $ LoggingT pure

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


--------------------------------------------------------------------------------
-- | The ResourceT version of Zeno
--------------------------------------------------------------------------------

type ZenoR r = ZenoT r (ResourceT IO)

runZenoR :: r -> ZenoR r a -> IO a
runZenoR r act = runResourceT $ runZeno r act

instance MonadResource (ZenoR r) where
  liftResourceT = lift . liftResourceT
