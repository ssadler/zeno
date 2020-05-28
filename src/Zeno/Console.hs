
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

import System.Console.ANSI.Codes
import System.Console.Concurrent
import System.Console.Regions

import Text.Printf

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
  , _cStep :: String
  }

makeLenses ''UI

emptyUIState = UI 0 [] ""


runConsoleUI :: ConsoleRegion -> Process ConsoleEvent -> Zeno r ()
runConsoleUI region proc = do

  spawn "UI Ticker" \_ -> do
    forever $ send proc UI_Tick >> threadDelay 200000

  liftIO do
    flip evalStateT emptyUIState do
      forever $ atomically (receiveSTM proc) >>= go

  where
  go UI_Tick = do
    peerEvents %= drop 1
    UI{..} <- get
    let
      peers = styleWith peersStyle $ printf " Peers: %i " _numPeers
      cstep = 
        if null _cStep
           then ""
           else styleWith [SetSwapForegroundBackground True] $ " " ++ _cStep ++ " "
    let r = peers ++ cstep :: String
    liftIO $ setConsoleRegion region r

  go (UI_NewPeer newTot) = do
    numPeers .= newTot
    peerEvents %= (++ [True])

  go (UI_DropPeer newTot) = do
    numPeers .= newTot
    peerEvents %= (++ [False])

  go (UI_ConsensusStep s) = do
    cStep .= s

  styleWith style s = setSGRCode style ++ s ++ setSGRCode [Reset]
  peersStyle =
    [ SetPaletteColor Background 198
    , SetPaletteColor Foreground 15
    , SetConsoleIntensity BoldIntensity
    ]


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
            localZeno (\app -> app { appConsole = Fancy (procMbox proc) }) act
