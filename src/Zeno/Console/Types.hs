
module Zeno.Console.Types where

import qualified Data.ByteString.Char8 as BS8
import Lens.Micro.Platform
import UnliftIO
import Control.Monad.Logger (LogLevel(..))

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

-- A bit mental, why not data with level, handle and maybe statusbar
data Console
  = PlainLog
  | Fancy (TBQueue ConsoleCtrl)
  | FilteredLog LogLevel Console


