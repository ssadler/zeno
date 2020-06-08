
module TestSecp256k1 where

import Network.Komodo

import Control.Monad
import Data.Serialize
import Network.Ethereum.Crypto

import Control.Concurrent
import Control.Concurrent.STM


runTestSecp256k1 :: IO ()
runTestSecp256k1 = do
  let sk = "4f3edf983ac636a65a842ce7c78d9aa706d3b113bce9c46f30d7d21715b23b1d" :: SecKey
  
  let atom = atomically
  count <- atom newEmptyTMVar

  let
    proc_a mbox mbox_b = do
      let KomodoIdent{..} = deriveKomodoIdent sk
      let message = sha3b $ encode kmdPubKeyI
      atom $ putTMVar mbox kmdPubKeyI
      atom do
        sig <- either error id . decode <$> takeTMVar mbox_b
        let Just addr = recoverAddr message sig
        when (addr /= deriveEthAddress kmdPubKey) do
          error "recovery failed"
        putTMVar count 1


    proc_b mbox mbox_b = do
      pk <- atom $ takeTMVar mbox
      let KomodoIdent{..} = deriveKomodoIdent sk
      when (pk /= kmdPubKeyI) do
        fail "derive failed"
      let message = sha3b $ encode pk
      atom $ putTMVar mbox_b $ encode $ sign sk message

    go = do
      mbox <- newEmptyTMVarIO
      mbox_b <- newEmptyTMVarIO
      forkIO $ proc_a mbox mbox_b
      forkIO $ proc_b mbox mbox_b

  let n = 1000000 :: Int
  forkIO do
    replicateM_ n go

  forM_ [0..n-1] \i -> do
    atom $ takeTMVar count
    when (mod i 10000 == 0) do print i

