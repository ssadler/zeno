
module Zeno.Console.Types where

import qualified Data.ByteString.Char8 as BS8
import UnliftIO
import Control.Monad.Logger (LogLevel(..))

import Lens.Micro.Platform

import Zeno.Data.FixedBytes


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
  , _statusBar :: Maybe (TBQueue ConsoleCtrl)
  , _writeStatusEvents :: Bool
  }

makeLenses ''Console

consoleWarn :: Console
consoleWarn = Console LevelWarn Nothing False

defaultLog :: Console
defaultLog = Console LevelDebug Nothing False


