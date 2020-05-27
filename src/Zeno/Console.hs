
module Zeno.Console
  ( Console(..)
  , ConsoleEvent(..)
  , withConsoleUI
  , sendUI
  ) where

import Control.Monad
import Control.Concurrent

import qualified Data.ByteString.Char8 as BS8
import Data.Function (fix)

import qualified System.Console.Terminfo as Term
import System.IO
import UnliftIO

import Zeno.Console.Types
import Zeno.Monad


sendUI :: ConsoleEvent -> Zeno r ()
sendUI evt = do
  getConsole >>=
    \case
      Fancy chan -> atomically (writeTQueue chan evt)
      _ -> mempty




withConsoleUI :: (Console -> IO a) -> IO a
withConsoleUI act = do
  queue <- newTQueueIO
  forkIO $ runConsoleUI queue
  finally (act (Fancy queue)) (atomically (writeTQueue queue UI_Quit))


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
          UI_Quit -> pure ()
          UI_LogLine line -> do
            putStr $ setRowAddress (nLines-1)
            putStr $ strBackColor Term.Blue "hello"

            putStr $ moveCursor $ Term.Point (nLines-2) 0
            BS8.putStrLn line
            go


runTestConsole = do
  withConsoleUI $ \(Fancy chan) -> do
    forM_ [0..] \i -> do
      atomically do
        let s = if mod i 2 == 0 then (BS8.replicate 200 '0') <> "\n" else "hello world\n"
        writeTQueue chan $ UI_LogLine s
      threadDelay 1000000


