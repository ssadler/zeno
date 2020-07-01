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
  , renderLogMessage
  , withLogCollect
  , pf
  , demoLogMessages
  ) where

import Control.Monad (when)
import Control.Monad.Logger as LOG hiding (logDebug, logInfo, logError, logWarn)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Lazy (toStrict)
import Data.String (fromString)
import Data.String.Conv
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Lens.Micro.Platform
import UnliftIO
import qualified Language.Haskell.Printf as Printf
import Language.Haskell.TH.Quote
import Text.Printf

import Zeno.Console.Types as LOG
import Zeno.Monad


instance MonadLogger (Zeno r) where
  monadLoggerLog a b c d = Zeno do
    console <- asks _console
    liftIO $ logMessage console a b c d


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


renderLogMessage :: Loc -> LogSource -> LogLevel -> LogStr -> IO BS8.ByteString
renderLogMessage loc source level str = do
  time <- formatTime defaultTimeLocale "[%d/%b/%y %T]" <$> getCurrentTime
  let s = toLogStr time <> levelStr <> toLogStr str <> "\n"
  pure $ fromLogStr s
  where
  sourceStr = if T.null source then "" else "(" <> source <> ")"
  levelStr = toLogStr $
    let f l = "[" <> l <> sourceStr <> "] " <> T.replicate (5-T.length l) " "
     in f $ case level of
          LevelOther t -> t
          _            -> toS $ drop 5 $ show level


logMessage :: ToLogStr msg => Console -> Loc -> LogSource -> LogLevel -> msg -> IO ()
logMessage (Console lvlFilter _debugMask mstatus _worker both collect) loc source level msg = do
  line <- renderLogMessage loc source level (toLogStr msg)

  let
    write = do
      BS8.hPutStr stdout line *> hFlush stdout
      when both do
        BS8.hPutStr stderr line *> hFlush stderr

    act = do
      case mstatus of
        Just queue -> atomically $ writeTBQueue queue $ UILog write
        Nothing -> write

  when (level >= lvlFilter) do
    case collect of
      Nothing -> act
      Just ref -> modifyIORef ref (>> act)


withLogCollect :: (Zeno r () -> Zeno r a) -> Zeno r a
withLogCollect act = do
  ref <- newIORef (pure ())
  let clear = liftIO $ modifyIORef ref \_ -> pure ()
  finally
    do localZeno (console . logCollect .~ Just ref) (act clear)
    do liftIO $ join $ readIORef ref


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
