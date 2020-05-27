
module Zeno.Console.Types where

import qualified Data.ByteString.Char8 as BS8
import UnliftIO


data Console =
    PlainLog
  | Fancy (TQueue ConsoleEvent)

data ConsoleEvent =
    UI_LogLine BS8.ByteString
  | UI_Quit
  | UI_NewPeer Int
  | UI_DropPeer Int
