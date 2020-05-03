
module Zeno.Consensus.Utils where

import Control.Distributed.Process
import Zeno.Prelude
import Zeno.Prelude.Lifted


repeatMatch :: Int -> [Match ()] -> Process ()
repeatMatch timeout matches = do
  startTime <- getCurrentTime
  fix $ \f -> do
    d <- timeDelta startTime
    let us = timeout - d
    when (us > 0) $
       receiveTimeout us matches >>= maybe (pure ()) (const f)


-- Spawns a process and links it to it's parent so that
-- it will die when it's parent dies
spawnLocalLink :: Process () -> Process ProcessId
spawnLocalLink proc = do
  myPid <- getSelfPid
  spawnLocal $ link myPid >> proc
