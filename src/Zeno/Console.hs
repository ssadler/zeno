
module Zeno.Console
  ( Console(..)
  , UI(..)
  , withConsoleUI
  , sendUI
  , withUIProc
  , renderStatus
  , testConsoleConcurrent
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Logger

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
import Zeno.Prelude

import System.Exit


sendUI :: ConsoleEvent -> Zeno r ()
sendUI evt = do
  getConsole >>=
    \c' ->
      fix1 c' \f ->
        \case
          FilteredLog _ console -> f console
          Fancy chan -> atomically (putTMVar chan $ UIEvent evt)
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
  sPeers = styleWith peersStyle $ printf " Peers: %i " _numPeers

  sStep =
    case _cStep of
      "" -> ""
      s -> printf " [%s]" s

  sProc =
    styleWith [SetConsoleIntensity BoldIntensity] $
      case _cProc of
        Nothing -> ""
        Just (UIRound label roundId) -> do
          (printf " [%s: %s]" (roundId) label) ++ sStep ++ " "
        Just (UIOther s) -> printf " [%s]" s

  styleWith style s = setSGRCode style ++ s ++ setSGRCode [Reset]
  peersStyle =
    [ SetPaletteColor Foreground 198
    , SetConsoleIntensity BoldIntensity
    ]



runConsoleUI :: ConsoleRegion -> Process ConsoleCtrl -> Zeno r ()
runConsoleUI region proc = do

  spawn "UI Ticker" \_ -> do
    forever $ send proc UITick >> threadDelay 200000

  liftIO do
    flip evalStateT emptyUIState do
      forever $ atomically (receiveSTM proc) >>= go

  where
  go UITick = do
    s <- renderStatus <$> get
    length s `seq` liftIO (setConsoleRegion region s)

  go (UIEvent evt) =
    case evt of
      UI_Peers n -> numPeers .= n
      UI_Process r -> cProc .= r
      UI_Step r  -> cStep .= r


withConsoleUI :: LogLevel -> Zeno r a -> Zeno r a
withConsoleUI level act = do
  withRunInIO \rio -> do
    displayConsoleRegions do
      withConsoleRegion Linear \region -> do
        rio do
          proc <- spawn "UI" $ runConsoleUI region
          let wrap = if level == LevelDebug then id else FilteredLog level
          let console = wrap $ Fancy (procMbox proc)
          localZeno (\app -> app { appConsole = console }) act


-- A random function for trying to debug the fancy console
testConsoleConcurrent :: IO ()
testConsoleConcurrent = do
  displayConsoleRegions do
    withConsoleRegion Linear \region -> do

      mbox <- newEmptyTMVarIO
      let
        run s = do
          o <- atomically (takeTMVar mbox)
          case o of
            Just s' -> run s'
            Nothing -> setConsoleRegion region s >> run s
      
      forkIO $ run ""
      forkIO $ forever $ atomically (putTMVar mbox Nothing) >> threadDelay 100000

      forever $ do
        withConcurrentOutput do
          outputConcurrent ("SomeLongWord\n" :: String)
        atomically $ do
          putTMVar mbox $ Just pinkWord
        withConcurrentOutput do
          outputConcurrent ("Hello2\n" :: String)
        threadDelay $ 50000
  where
  pinkWord = setSGRCode [SetConsoleIntensity BoldIntensity, SetPaletteColor Foreground 198]
               ++ "I'm pink" ++ setSGRCode [Reset]
