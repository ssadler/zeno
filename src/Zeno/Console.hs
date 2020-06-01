
module Zeno.Console
  ( Console(..)
  , UI(..)
  , withConsoleUI
  , sendUI
  , withUIProc
  , renderStatus
  , testConsole
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Logger hiding (logInfo)

import qualified Data.ByteString.Char8 as BS8
import Data.Function (fix)

import Lens.Micro.Platform

import System.Console.ANSI.Codes

import Text.Printf

import UnliftIO
import UnliftIO.Concurrent

import Zeno.Console.Types
import Zeno.Monad
import Zeno.Process
import Zeno.Prelude

import System.Console.ANSI
import System.Exit


sendUI :: ConsoleEvent -> Zeno r ()
sendUI evt = do
  getConsole >>=
    \c' ->
      fix1 c' \f ->
        \case
          FilteredLog _ console -> f console
          Fancy chan -> atomically (writeTBQueue chan $ UIEvent evt)
          _ -> mempty

withUIProc :: UIProcess -> Zeno r a -> Zeno r a
withUIProc proc act = do
  sendUI $ UI_Process $ Just proc
  finally act $ sendUI $ UI_Process Nothing


data UI = UI
  { _numPeers :: Int
  , _cProc :: Maybe UIProcess
  , _cStep :: String
  }

makeLenses ''UI

emptyUIState = UI 0 Nothing ""


renderStatus :: UI -> String
renderStatus UI{..} = sPeers ++ sProc
  where 
  sPeers = styleWith peersStyle $ printf "[Peers: %i]" _numPeers

  sStep =
    case _cStep of
      "" -> ""
      s -> printf "[%s]" s

  sProc =
    styleWith [SetConsoleIntensity BoldIntensity] $
      case _cProc of
        Nothing -> ""
        Just (UIRound label roundId) -> do
          (printf "[%s: %s]" (roundId) label) ++ sStep ++ " "
        Just (UIOther s) -> printf "[%s]" s

  styleWith style s = setSGRCode style ++ s ++ setSGRCode [Reset]
  peersStyle =
    [ SetPaletteColor Foreground 198
    , SetConsoleIntensity BoldIntensity
    ]


runConsoleUI :: Process ConsoleCtrl -> Zeno r ()
runConsoleUI proc = do

  spawn "UI Ticker" \_ -> do
    forever $ send proc UITick >> threadDelay 200000

  liftIO do
    flip evalStateT emptyUIState do
      forever $ atomically (receiveSTM proc) >>= go

  where
  go :: ConsoleCtrl -> StateT UI IO ()
  go (UILog line) = do
    log line
    tick

  go UITick = do
    tick

  go (UIEvent evt) =
    case evt of
      UI_Peers n -> numPeers .= n
      UI_Process r -> cProc .= r
      UI_Step r  -> cStep .= r

  log line = do
    liftIO do
      clearLine
      setCursorColumn 0
      BS8.putStr line           -- Line is assumed to include newline in this case

  tick = do
    s <- renderStatus <$> get
    liftIO do
      showCursor
      clearLine
      setCursorColumn 0
      putStr s
      hFlush stdout


withConsoleUI :: LogLevel -> Zeno r a -> Zeno r a
withConsoleUI level act = do
  proc <- spawn "UI" runConsoleUI
  let wrap = if level == LevelDebug then id else FilteredLog level
  let console = wrap $ Fancy $ procMbox proc
  localZeno (\app -> app { appConsole = console }) act


testConsole :: IO ()
testConsole = do
  runZeno PlainLog () do
    withConsoleUI LevelDebug do
      forM_ [0..] \i -> do
        sendUI $ UI_Peers i
        when (mod i 3 == 0) do
          logInfo $ "i is getting longer: " ++ concat (replicate i (show i))
        threadDelay 400000

