
module Zeno.Console
  ( Console(..)
  , MonadLoggerUI(..)
  , UI(..)
  , withConsole
  , renderStatus
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Logger hiding (logInfo)

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Set as Set

import System.Console.ANSI.Codes
import System.Console.ANSI
import System.IO (hPutStr)
import System.Exit

import Text.Printf

import UnliftIO
import UnliftIO.Concurrent

import Zeno.Console.Types
import Zeno.Monad
import Zeno.Process
import Zeno.Prelude


class (MonadUnliftIO m, MonadLogger m) => MonadLoggerUI m where
  sendUI :: ConsoleEvent -> m ()

instance MonadLoggerUI (Zeno r) where
  sendUI evt = do
    getConsole >>=
      \case
        Console _ _ (Just chan) _ _ ->
          atomically (writeTBQueue chan $ UIEvent evt)
        _ -> pure ()


data UI = UI
  { _numPeers :: Int
  , _cProc :: Maybe UIProcess
  , _cStep :: String
  , _cMofN :: (Int, Int)
  }

makeLenses ''UI

emptyUIState :: UI
emptyUIState = UI 0 Nothing "" (0, 0)


renderStatus :: UI -> String
renderStatus UI{..} = sPeers ++ sProc
  where 
  sPeers = styleWith peersStyle $ printf "[Peers: %i]" _numPeers

  sStep =
    case _cStep of
      "" -> ""
      s -> printf "[%s]" s

  sMofN = case _cMofN of (_, 0) -> ""; o -> "[%i of %i]" % o

  sProc =
    styleWith [SetConsoleIntensity BoldIntensity] $
      case _cProc of
        Nothing -> ""
        Just (UIRound label roundId) ->
          let roundIdShort = take 7 $ show roundId
           in printf "[%s: %s]" roundIdShort label ++ sStep ++ sMofN ++ " "
        Just (UIOther s) -> printf "[%s]" s

  styleWith style s = setSGRCode style ++ s ++ setSGRCode [Reset]
  peersStyle =
    [ SetPaletteColor Foreground 198
    , SetConsoleIntensity BoldIntensity
    ]


runConsoleUI :: Process ConsoleCtrl -> Zeno r ()
runConsoleUI proc = do

  spawn "UI Ticker" \_ -> do
    forever $ send proc UITick >> threadDelay 200000

  liftIO do
    flip evalStateT emptyUIState do
      forever $ atomically (receiveSTM proc) >>= go

  where
  handle = stdout
  go :: ConsoleCtrl -> StateT UI IO ()
  go (UILog act) = do
    liftIO do
      hClearLine handle
      hSetCursorColumn handle 0
      act                  -- Action writing lines with newline
    go UITick

  go UITick = do
    s <- renderStatus <$> get
    liftIO do
      -- showCursor
      hClearLine handle
      hSetCursorColumn handle 0
      hPutStr handle s
      hFlush handle

  go (UIEvent evt) = do
    case evt of
      UI_Peers n -> numPeers .= n
      UI_Process r -> cProc .= r
      UI_Step r  -> cStep .= r >> cMofN .= (0, 0)
      UI_MofN m n -> cMofN .= (m, n)


withConsole :: ConsoleArgs -> LogLevel -> Zeno r a -> Zeno r a
withConsole (useUI, debug, both) level act = do
  let topics = Set.fromList $ T.splitOn "," $ T.toLower $ T.pack debug
  ui <- 
    if useUI
       then Just . procMbox <$> spawn "UI" runConsoleUI
       else pure Nothing

  let c = Console level topics ui "" both
  localZeno (console .~ c) act


-- testConsole :: IO ()
-- testConsole = do
--   runZeno defaultLog () do
--     withConsoleUI LevelDebug do
--       forM_ [0..] \i -> do
--         sendUI $ UI_Peers i
--         when (mod i 3 == 0) do
--           logInfo $ "i is getting longer: " ++ concat (replicate i (show i))
--         threadDelay 400000
