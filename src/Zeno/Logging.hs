{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Zeno.Logging 
  ( module LOG
  , logDebug
  , logInfo
  , logError
  , logWarn
  , logMurphy
  , logMessage
  , AsString
  , asString
  , getLogMessage
  , pf
  ) where

import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.Time.Clock
import Data.Time.Format
import Control.Monad.Logger as LOG hiding (logDebug, logInfo, logError, logWarn)
import UnliftIO
import qualified Language.Haskell.Printf as Printf
import Language.Haskell.TH.Quote

import Zeno.Console.Types as LOG


logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugN . fromString


logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . fromString

logError :: MonadLogger m => String -> m ()
logError = logErrorN . fromString

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . fromString

logMurphy :: MonadLogger m => String -> m ()
logMurphy s = logErrorN $ "Invariant violated: " <> fromString s

class AsString a where
  asString :: a -> String

instance AsString BS8.ByteString where
  asString = BS8.unpack

instance AsString Value where
  asString = asString . toStrict . encode


getLogMessage :: Loc -> LogSource -> LogLevel -> LogStr -> IO BS8.ByteString
getLogMessage loc source level str = do
  t <- formatTime defaultTimeLocale "[%T]" <$> getCurrentTime
  pure $ fromLogStr $ toLogStr t <> defaultLogStr loc source level str

logMessage :: ToLogStr msg => Console -> Loc -> LogSource -> LogLevel -> msg -> IO ()
logMessage (Console lvlFilter mstatus _ h) loc source level msg = do
  line <- getLogMessage loc source level (toLogStr msg)
  when (level >= lvlFilter) do
    case mstatus of
      Just queue -> atomically $ writeTBQueue queue $ UILog line
      Nothing -> BS8.hPutStr h line *> hFlush stdout


pf :: QuasiQuoter
pf = Printf.s
