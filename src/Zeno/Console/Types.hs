
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
  | UILog (IO ())

data ConsoleEvent
  = UI_Peers Int
  | UI_Process (Maybe UIProcess)
  | UI_Step String
  | UI_MofN Int Int
  | UI_Tick
  deriving (Show)

data UIProcess
  = UIRound String Bytes11
  | UIOther String
  deriving (Show)

data Console = Console
  { _logLevel     :: LogLevel
  , _logDebugMask :: Set.Set Text
  , _statusBar    :: Maybe (TBQueue ConsoleCtrl)
  , _logWorker    :: String -- unused
  , _logBothFH    :: Bool
  , _logCollect   :: Maybe (IORef (IO ()))
  }

makeLenses ''Console

debugTraceRPC :: Text
debugTraceRPC = "rpc"

consoleWarn :: Console
consoleWarn = Console LevelWarn mempty Nothing "" False Nothing

defaultLog :: Console
defaultLog = Console LevelDebug mempty Nothing "" False Nothing

type ConsoleArgs = (Bool, String, Bool)

optConsoleArgs = (,,) <$> ui <*> pure "" <*> both
  where
  ui = switch ( long "log-ui" <> help "Enable ghastly status bar" )
  both = switch
     ( long "log-both"
    <> help "For use with --log-ui so that stderr can be piped, ie: 2>zeno.log" )
