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
  , logTrace
  , whenSlow
  , getLogMessage
  , pf
  , demoLogMessages
  ) where

import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Control.Monad.Reader
import Control.Monad.Logger as LOG hiding (logDebug, logInfo, logError, logWarn)
import UnliftIO
import qualified Language.Haskell.Printf as Printf
import Language.Haskell.TH.Quote
import Text.Printf

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

logTrace :: MonadLogger m => Text -> String -> m ()
logTrace level = logOtherN (LevelOther level) . fromString


getLogMessage :: Loc -> LogSource -> LogLevel -> LogStr -> IO BS8.ByteString
getLogMessage loc source level str = do
  time <- formatTime defaultTimeLocale "[%d/%b/%y %T]" <$> getCurrentTime
  let s = toLogStr time <> "[" <> levelStr <> sourceStr <> "] " <> toLogStr str <> "\n"
  pure $ fromLogStr s
  where
  sourceStr = toLogStr $ if T.null source then "" else "(" <> source <> ")"
  levelStr =
    case level of
      LevelOther t -> toLogStr t
      _            -> toLogStr $ BS8.pack $ drop 5 $ show level


logMessage :: ToLogStr msg => Console -> Loc -> LogSource -> LogLevel -> msg -> IO ()
logMessage (Console lvlFilter _debugMask mstatus _ h worker) loc source level msg = do
  line <- getLogMessage loc source level (toLogStr msg)

  let doLog = level >= lvlFilter

  when doLog do
    case mstatus of
      Just queue -> atomically $ writeTBQueue queue $ UILog line
      Nothing -> BS8.hPutStr h line *> hFlush stdout


whenSlow :: MonadIO m => Int -> m a -> (Int -> m ()) -> m a
whenSlow threshold act log = do
  startTime <- liftIO $ getCurrentTime
  r <- act
  endTime <- liftIO $ getCurrentTime
  let t = diffUTCTime endTime startTime
  let ms = round $ realToFrac t * 1000
  when (ms >= threshold) do log ms
  pure r




pf :: QuasiQuoter
pf = Printf.s

newtype TestLogger a = TestLogger (ReaderT Console IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runTestLogger (TestLogger r) = runReaderT r

instance MonadLogger TestLogger where
  monadLoggerLog a b c d = TestLogger do
    console <- ask
    liftIO $ logMessage console a b c d

demoLogMessages :: IO ()
demoLogMessages = do
  flip runTestLogger defaultLog do
    logInfo "hi"
    logError "hi"
    logWarnNS "rpc" "wat"
