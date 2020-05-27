
module Zeno.Console where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

import qualified Data.ByteString.Char8 as BS8
import Data.Function (fix)

import qualified System.Console.Terminfo as Term
import System.IO

import Zeno.Logging


data ConsoleInterface =
    PlainLog
  | UI (TQueue ConsoleEvent)

data ConsoleEvent =
    LogLine BS8.ByteString
  | Quit


logMessage :: ToLogStr msg
           => ConsoleInterface -> Loc -> LogSource -> LogLevel -> msg -> IO ()
logMessage cif loc source level msg = do
  line <- getLogMessage loc source level (toLogStr msg)
  case cif of
    PlainLog -> BS8.hPutStr stderr line
    UI queue -> atomically (writeTQueue queue $ LogLine line)

withConsoleUI :: (ConsoleInterface -> IO a) -> IO a
withConsoleUI act = do
  queue <- newTQueueIO
  forkIO $ runConsoleUI queue
  finally (act (UI queue)) (atomically (writeTQueue queue Quit))


runConsoleUI :: TQueue ConsoleEvent -> IO ()
runConsoleUI inbox = do
  Term.setupTermFromEnv >>= outer
  where
  outer term = do
    let
      termFunc = Term.getCapability term
      Just nLines = termFunc Term.termLines
      Just nCols = termFunc Term.termColumns
      Just strWithColor = termFunc Term.withForegroundColor :: Maybe (Term.Color -> String -> String)
      Just moveCursor = termFunc Term.cursorAddress
      Just setRowAddress = termFunc Term.rowAddress :: Maybe (Int -> String)
      Just setColAddress = termFunc Term.columnAddress :: Maybe (Int -> String)
      Just clearEOS = termFunc Term.clearEOS
      Just strBackColor = termFunc Term.withBackgroundColor :: Maybe (Term.Color -> String -> String) 
      


    fix \go -> do
      atomically (readTQueue inbox) >>=
        \case
          Quit -> pure ()
          LogLine line -> do
            putStr $ setRowAddress (nLines-1)
            putStr $ strBackColor Term.Blue "hello"

            putStr $ moveCursor $ Term.Point (nLines-2) 0
            BS8.putStrLn line




            go

runTestConsole = do
  withConsoleUI $ \(UI chan) -> do
    forM_ [0..] \i -> do
      atomically do
        let s = if mod i 2 == 0 then (BS8.replicate 200 '0') <> "\n" else "hello world\n"
        writeTQueue chan $ LogLine s
      threadDelay 1000000


