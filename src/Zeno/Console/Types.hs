
module Zeno.Console.Types where

import qualified Data.ByteString.Char8 as BS8
import Lens.Micro.Platform
import UnliftIO

import Zeno.Data.FixedBytes


data ConsoleCtrl =
    UITick
  | UIEvent ConsoleEvent

data ConsoleEvent =
    UI_Peers Int
  | UI_Process (Maybe UIProcess)
  | UI_Step String
  | UI_Tick

data UIProcess =
    UIRound String Bytes6 
  | UIOther String

data Console =
    PlainLog
  | Fancy (TMVar ConsoleCtrl)


