
module Zeno.Console
  ( Console(..)
  , ConsoleEvent(..)
  , withConsoleUI
  , sendUI
  , runTestConsole
  ) where

import Control.Monad
import Control.Monad.State

import qualified Data.ByteString.Char8 as BS8
import Data.Function (fix)

import Lens.Micro.Platform

import System.Console.Concurrent
import System.Console.Regions
--import System.IO
import UnliftIO
import UnliftIO.Concurrent

import Zeno.Console.Types
import Zeno.Monad
import Zeno.Process




sendUI :: ConsoleEvent -> Zeno r ()
sendUI evt = do
  getConsole >>=
    \case
      Fancy chan -> atomically (putTMVar chan evt)
      _ -> mempty


data UI = UI
  { _numPeers :: Int
  , _peerEvents :: [Bool]
  , _uiCache :: String
  }

makeLenses ''UI

emptyUIState = UI 0 [] ""


runConsoleUI :: ConsoleRegion -> Process ConsoleEvent -> Zeno r ()
runConsoleUI region proc = do

  spawn "UI Ticker" \_ -> do
    forever $ send proc UI_Tick >> threadDelay 100000

  liftIO $ evalStateT go emptyUIState

  where
  go :: StateT UI IO ()
  go = do

    let 
      update = do
        UI{..} <- get
        let cmd = " Peers: " ++ show _numPeers
        uiCache .= cmd

      render = do
        use uiCache >>= liftIO . setConsoleRegion region

    evt <- atomically $ receiveSTM proc
    case evt of
      UI_Quit -> pure ()

      UI_Tick -> do
        update
        render
        go

      UI_NewPeer newTot -> do
        numPeers .= newTot
        peerEvents %= (++ [True])
        go

      UI_DropPeer newTot -> do
        numPeers .= newTot
        peerEvents %= (++ [False])
        go


-- TODO: Refactor console so that it gets it's common interface from Logging
-- (make dot | xdot -)
      





runTestConsole = do
  runZeno PlainLog () do
    withConsoleUI do
      forM_ [0..] \i -> do
        let s = if mod i 2 == 0 then "Hi " <> (BS8.replicate 200 '0') <> "\n" else "hello world\n"
        --sendUI $ UI_Log s
        threadDelay 1000000




withConsoleUI :: Zeno r a -> Zeno r a
withConsoleUI act = do
  withRunInIO \rio -> do
    withConcurrentOutput do
      displayConsoleRegions do
        withConsoleRegion Linear \region -> do
          rio do
            proc <- spawn "UI" $ runConsoleUI region
            let run = localZeno (\app -> app { appConsole = Fancy (procMbox proc) }) act
            finally run $ send proc UI_Quit
