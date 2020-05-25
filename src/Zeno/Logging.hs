{-# LANGUAGE OverloadedStrings #-}

module Zeno.Logging 
  ( module LOG
  , logDebug
  , logInfo
  , logError
  , logWarn
  , logTime
  , logStderr
  , AsString
  , asString
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Control.Monad.IO.Class as ALL (liftIO, MonadIO)
import Control.Monad.Logger as LOG hiding (logDebug, logInfo, logError, logWarn)
import System.IO
import System.IO.Unsafe

import Data.String (fromString)

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

logStderr :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logStderr loc source level str = do
  t <- formatTime defaultTimeLocale "[%T]" <$> getZonedTime
  BS8.hPutStr stderr $ fromLogStr $ toLogStr t <> defaultLogStr loc source level str
  where
  logStr = defaultLogStr

logTime :: (MonadIO m, MonadLogger m) => String -> m a -> m a
logTime s act = do
  startTime <- liftIO $ getCurrentTime
  r <- act
  endTime <- liftIO $ getCurrentTime
  let t = diffUTCTime endTime startTime
  logDebug $ s ++ " took: " ++ (show $ round $ (realToFrac t) * 1000) ++ "ms"
  pure r

