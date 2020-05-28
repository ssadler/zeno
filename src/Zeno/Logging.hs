{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

module Zeno.Logging 
  ( module LOG
  , logDebug
  , logInfo
  , logError
  , logWarn
  , logTime
  , logMessage
  , AsString
  , asString
  , getLogMessage
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.String.Conv
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Control.Monad.Logger as LOG hiding (logDebug, logInfo, logError, logWarn)
import System.Console.Concurrent
import UnliftIO

import Zeno.Console.Types as LOG


logDebug :: MonadLogger m => String -> m ()
logDebug = logDebugN . fromString

logInfo :: MonadLogger m => String -> m ()
logInfo = logInfoN . fromString

logError :: MonadLogger m => String -> m ()
logError = logErrorN . fromString

logWarn :: MonadLogger m => String -> m ()
logWarn = logWarnN . fromString

class AsString a where
  asString :: a -> String

instance AsString BS8.ByteString where
  asString = BS8.unpack

instance AsString Value where
  asString = asString . toStrict . encode

getLogMessage :: Loc -> LogSource -> LogLevel -> LogStr -> IO BS8.ByteString
getLogMessage loc source level str = do
  t <- formatTime defaultTimeLocale "[%T]" <$> getZonedTime
  pure $ fromLogStr $ toLogStr t <> defaultLogStr loc source level str

logMessage :: ToLogStr msg => Console -> Loc -> LogSource -> LogLevel -> msg -> IO ()
logMessage console loc source level msg = do
  line <- getLogMessage loc source level (toLogStr msg)
  case console of
    PlainLog -> BS8.hPutStr stderr line
    Fancy queue -> outputConcurrent (toS line :: Text)

logTime :: (MonadIO m, MonadLogger m) => String -> m a -> m a
logTime s act = do
  startTime <- liftIO $ getCurrentTime
  r <- act
  endTime <- liftIO $ getCurrentTime
  let t = diffUTCTime endTime startTime
  logDebug $ s ++ " took: " ++ (show $ round $ (realToFrac t) * 1000) ++ "ms"
  pure r

