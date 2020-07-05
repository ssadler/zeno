
module Zeno.Signal
  ( module Zeno.Signal
  , sigUSR1
  ) where

import qualified Data.Map as Map
import System.Posix.Signals

import System.IO.Unsafe
import UnliftIO


signalHandlers :: IORef (Map.Map Signal (IO ()))
signalHandlers = unsafePerformIO $ newIORef mempty

installSignalHandler :: MonadUnliftIO m => Signal -> m () -> m ()
installSignalHandler sig !handler = do
  rio <- askRunInIO
  let h = rio handler
  m <- atomicModifyIORef signalHandlers \m -> (Map.insertWith (>>) sig h m, m)
  case Map.lookup sig m of
    Nothing -> do
      liftIO $ installHandler sig (Catch $ runSignals sig) Nothing
      pure ()
    _ -> pure ()

runSignals :: Signal -> IO ()
runSignals sig = do
  m <- readIORef signalHandlers
  maybe mempty id $ Map.lookup sig m
