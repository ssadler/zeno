
module Zeno.Console.Types where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Logger (LogLevel(..))
import Options.Applicative
import qualified Data.Set as Set
import Data.Text

import Lens.Micro.Platform

import System.IO
import UnliftIO

import Data.FixedBytes


data ConsoleCtrl
  = UITick
  | UIEvent ConsoleEvent
  | UILog BS8.ByteString

data ConsoleEvent
  = UI_Peers Int
  | UI_Process (Maybe UIProcess)
  | UI_Step String
  | UI_Tick
  deriving (Show)

data UIProcess
  = UIRound String Bytes6 
  | UIOther String
  deriving (Show)

data Console = Console
  { _logLevel :: LogLevel
  , _logDebugMask :: Set.Set Text
  , _statusBar :: Maybe (TBQueue ConsoleCtrl)
  , _writeStatusEvents :: Bool
  , _fileHandle :: Handle
  }

makeLenses ''Console

debugTraceRPC :: Text
debugTraceRPC = "rpc"

consoleWarn :: Console
consoleWarn = Console LevelWarn mempty Nothing False stdout

defaultLog :: Console
defaultLog = Console LevelDebug mempty Nothing False stdout

stderrLog :: Console
stderrLog = Console LevelDebug mempty Nothing False stderr

type ConsoleArgs = (Bool, String)

optConsoleArgs = (,) <$> ui <*> debug
  where
  ui = switch
     ( long "log-ui"
    <> help "Enable fancy (but currently buggy) status bar" )
  debug = option str
     ( long "log-debug"
    <> value mempty
    <> help "comma separated list of topics to debug i.e: rpc" )

