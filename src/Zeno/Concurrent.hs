
module Zeno.Concurrent where


import           Control.Exception (AsyncException(ThreadKilled))
import           Control.Concurrent
import           Control.Exception.Safe as E
import           Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Zeno.Prelude

-- Parallel computation

parM_ :: Int -> [a] -> (a -> Zeno r b) -> Zeno r ()
parM_ s i a = parM s i a >> pure ()

parM :: Int -> [a] -> (a -> Zeno r b) -> Zeno r [b]
parM slots items act = do
  r <- ask
  mvar <- liftIO newEmptyMVar
  
  let fork (i,o) = do
        let ef = do
              myId <- myThreadId
              let mres = (,,) i <$> myThreadId <*> runZeno r (act o)
              handleAny (pure . Left) $ Right <$> mres
        let quietly = handleAsync (\ThreadKilled -> pure ())
        forkIO $ quietly $ ef >>= putMVar mvar

  let run workers rmap rest
        | Set.null workers && null rest = do
            pure $ snd <$> Map.toAscList rmap
        | Set.size workers < slots && not (null rest) = do
            let (j:xs) = rest
            pid <- fork j
            run (Set.insert pid workers) rmap xs
        | otherwise = do
            takeMVar mvar >>=
              \case Right (i, pid, res) -> do
                      let nmap = Map.insert i res rmap
                      run (Set.delete pid workers) nmap rest
                    Left e -> do
                      mapM_ killThread workers
                      throw e

  liftIO $ run Set.empty Map.empty $ zip [0..] items
