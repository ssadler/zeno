
module Zeno.Process.Node.InboundRateLimit
  ( ReceiverMap
  , ClassyAsync
  , classyAsync
  , newReceiverMap
  , readReceiverMap
  , inboundConnectionLimit
  , testInboundConnectionLimit
  ) where

import Data.Word
import Control.Monad
import Control.Monad.Catch
import Test.DejaFu
import Test.DejaFu.Conc.Internal.STM
import Test.DejaFu.Conc.Internal.Common

import Control.Monad.Conc.Class
import Control.Concurrent.Classy hiding (wait)
import Control.Concurrent.Classy.Async
import Control.Concurrent.Classy.MVar

import qualified Data.Map as Map


type HostAddress = Word32
type ReceiverMap m = MVar m (Map.Map HostAddress Int)
type ClassyAsync = Async IO
classyAsync :: MonadConc m => m a -> m (Async m a)
classyAsync = async

newReceiverMap :: MonadConc m => m (ReceiverMap m)
newReceiverMap = newMVar mempty

readReceiverMap :: MonadConc m => ReceiverMap m -> m (Map.Map HostAddress Int)
readReceiverMap = readMVar

inboundConnectionLimit
  :: forall m a. MonadConc m
  => ReceiverMap m
  -> HostAddress
  -> Int
  -> m ()
  -> m ()
inboundConnectionLimit mreceivers ip maxConn act = do
  Control.Concurrent.Classy.mask \unmask -> do
    r <-
      modifyMVar mreceivers
        \rmap -> do
          case Map.lookup ip rmap of
            Nothing -> pure (Map.insert ip 1 rmap, True)
            Just n | n == maxConn -> pure (rmap, False)
            Just n -> pure (Map.insert ip (n+1) rmap, True)
    when r do
       finally
        do unmask act
        do
          modifyMVarMasked_ (mreceivers :: ReceiverMap m)
            \rmap ->
              pure $
                case (Map.lookup ip rmap) of
                  Nothing -> error "Invariant: Receivers map got Nothing"
                  Just 1 -> Map.delete ip rmap
                  Just n -> Map.insert ip (n-1) rmap


testInboundConnectionLimit :: Program Basic IO ()
testInboundConnectionLimit = do

  mreceivers <- newMVar mempty
  sem <- newMVar (0 :: Int)

  asyncs <-
    forM [0..1] \i -> do
      async do
        inboundConnectionLimit mreceivers 0 1 do
          modifyMVar_ sem \p -> do
            when (p /= 0) $ error $ "invariant violated: " ++ show p
            pure (p+1)

          modifyMVar_ sem (pure . (subtract 1))

  forM_ asyncs \a -> do
    waitCatch a >>= either (fail . show) pure

data TooManyThreads = TooManyThreads deriving (Show)
instance Exception TooManyThreads
