
module Zeno.Process.Node.InboundRateLimit
  ( ReceiverMap
  , ClassyAsync
  , classyAsync
  , newReceiverMap
  , inboundConnectionLimit
  , testInboundConnectionLimit
  ) where

import Control.Monad
import Control.Monad.Catch
import Network.Socket (HostAddress)
import Test.DejaFu
import Test.DejaFu.Conc.Internal.Common
import Test.DejaFu.Conc.Internal.STM

import Control.Monad.Conc.Class
import Control.Concurrent.Classy hiding (wait)
import Control.Concurrent.Classy.Async
import Control.Concurrent.Classy.MVar

import Network.Socket (HostAddress)
import qualified Data.Map as Map


type ReceiverMap m = TVar (STM m) (Map.Map HostAddress (Async m ()))
type ClassyAsync = Async IO
classyAsync :: MonadConc m => m a -> m (Async m a)
classyAsync = async

newReceiverMap :: MonadConc m => m (ReceiverMap m)
newReceiverMap = atomically (newTVar mempty)


inboundConnectionLimit
  :: MonadConc m
  => ReceiverMap m
  -> HostAddress
  -> Async m ()
  -> m a
  -> m a
inboundConnectionLimit mreceivers ip asnc act = do
  let
    run = do
      mapM_ cancel =<< atomically do
        lookupAsync ip mreceivers <* insertAsync ip asnc mreceivers
      act

  finally run do
      atomically do
        lookupAsync ip mreceivers >>= \case
          Nothing -> pure ()
          Just oasnc -> do
            when (asnc == oasnc) (void $ deleteAsync ip mreceivers)


insertAsync :: MonadConc m => HostAddress -> Async m () -> ReceiverMap m -> STM m ()
insertAsync ip asnc t = do
  modifyTVar t $ Map.insert ip asnc

lookupAsync :: MonadConc m => HostAddress -> ReceiverMap m -> STM m (Maybe (Async m ()))
lookupAsync ip tmap = do
  Map.lookup ip <$> readTVar tmap

deleteAsync :: MonadConc m => HostAddress -> ReceiverMap m -> STM m ()
deleteAsync ip t = modifyTVar t $ Map.delete ip


testInboundConnectionLimit :: Program (WithSetup (ModelTVar IO Integer)) IO ()
testInboundConnectionLimit = withSetup setup \sem -> do

  mreceivers <- atomically (newTVar mempty)

  asyncs <- forM [0..2] \i -> do
      handoff <- newEmptyMVar
      asnc <- async do
        me <- takeMVar handoff
        inboundConnectionLimit mreceivers 0 me do
          (`finally` atomically (modifyTVar sem (subtract 1))) do
            threadDelay 1
            atomically $ modifyTVar sem (+1)
      putMVar handoff asnc
      pure asnc

  mapM_ waitCatch asyncs

  where
  setup :: Program Basic IO (ModelTVar IO Integer)
  setup = do
    single <- atomically $ newTVar 0
    registerInvariant do
      n <- inspectTVar single
      when (n > 1) $ error ("too many threads: " ++ show n)
      pure ()
    pure single
