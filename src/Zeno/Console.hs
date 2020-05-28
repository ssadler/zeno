
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

import qualified System.Console.Terminal.Size as Term
import qualified System.Console.Terminfo as Term
import System.IO
import System.Posix.Signals
import System.Posix.Signals.Exts
import UnliftIO
import UnliftIO.Concurrent

import Zeno.Console.Types
import Zeno.Monad
import Zeno.Process



-- TODO: Refactor console so that it gets it's common interface from Logging
-- (make dot | xdot -)


data UI = UI
  { _numPeers :: Int
  , _peerEvents :: [Bool]
  , _uiCache :: String
  }

makeLenses ''UI

emptyUIState = UI 0 [] ""





sendUI :: ConsoleEvent -> Zeno r ()
sendUI evt = do
  getConsole >>=
    \case
      Fancy chan -> atomically (putTMVar chan evt)
      _ -> mempty




withConsoleUI :: Zeno r a -> Zeno r a
withConsoleUI act = do
  proc <- spawn "UI" runConsoleUI
  let run = localZeno (\app -> app { appConsole = Fancy (procMbox proc) }) act
  finally run (send proc UI_Quit)


runConsoleUI :: Process ConsoleEvent -> Zeno r ()
runConsoleUI proc = do

  spawn "UI Ticker" \_ -> do
    forever do send proc UI_Tick >> threadDelay 100000

  liftIO do
    installHandler sigWINCH (Catch $ send proc $ UI_Winch) Nothing
    finally
      (putStr "\ESC[?25l" >> evalStateT outer emptyUIState)
      (putStr "\ESC[?25h")

  where
  
  outer :: StateT UI IO ()
  outer = do
    Just (Term.Window nLines nCols) <- liftIO Term.size
    term <- liftIO Term.setupTermFromEnv
    let
      termFunc = Term.getCapability term
      Just moveCursor = termFunc Term.cursorAddress
      Just setRowAddress = termFunc Term.rowAddress :: Maybe (Int -> String)
      Just setColAddress = termFunc Term.columnAddress :: Maybe (Int -> String)
      Just clearEOL = termFunc Term.clearEOL :: Maybe String
      Just clearEOS = termFunc Term.clearEOS
      Just strWithColor = termFunc Term.withForegroundColor :: Maybe (Term.Color -> String -> String)
      Just strBackColor = termFunc Term.withBackgroundColor :: Maybe (Term.Color -> String -> String)

      update = do
        UI{..} <- get
        let s = take nCols $ " Peers: " ++ show nCols
        let cmd = (moveCursor $ Term.Point nLines 0) ++
                  (strWithColor Term.Blue s)
        uiCache .= cmd

      render = do
        use uiCache >>= liftIO . putStr

    liftIO $ print nCols

    fix \go -> do
      evt <- atomically $ receiveSTM proc
      case evt of
        UI_Quit -> pure ()

        UI_Winch -> do
          send proc $ UI_Log "Winched!\n"
          outer

        UI_Log line -> do
          liftIO do
            -- putStr $ moveCursor $ Term.Point (nLines-2) 0
            BS8.putStr line
            --Term.runTermOutput term $ clearEOS 1000
          -- render
          go

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

      





runTestConsole = do
  runZeno PlainLog () do
    withConsoleUI do
      forM_ [0..] \i -> do
        let s = if mod i 2 == 0 then "Hi " <> (BS8.replicate 200 '0') <> "\n" else "hello world\n"
        --sendUI $ UI_Log s
        threadDelay 1000000


