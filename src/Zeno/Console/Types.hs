
module Zeno.Console.Types where

import qualified Data.ByteString.Char8 as BS8
import UnliftIO


data Console =
    PlainLog
  | Fancy (TMVar ConsoleEvent)

data ConsoleEvent =
    UI_Log BS8.ByteString
  | UI_Quit
  | UI_Tick
  | UI_Winch
  | UI_NewPeer Int
  | UI_DropPeer Int
