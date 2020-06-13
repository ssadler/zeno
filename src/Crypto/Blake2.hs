
module Crypto.Blake2
  ( blake2bPersonalized
  , doDelayThingyManyThreads
  ) where


import Data.ByteArray as B
import Data.ByteString as BS
import Data.Word
import Foreign.Ptr (Ptr)
import System.IO.Unsafe

import UnliftIO
import UnliftIO.Concurrent
import Control.Monad

foreign import ccall unsafe "blake2b_256_personalized"
    c_blake2b_256_personalized :: Ptr Word8 -> Int  --- Pointer and length of personalization
                               -> Ptr Word8 -> Int  --- Pointer and length of data
                               -> Ptr Word8         --- Pointer to output
                               -> IO Int

blake2bPersonalized :: ByteString -> ByteString -> ByteString
blake2bPersonalized personalization input
  | BS.length personalization > 16 = error "blake2b personalization too long"
  | otherwise =
      unsafePerformIO $ do
        B.create 32 $ \outPtr -> do
          B.withByteArray personalization $ \perPtr -> do
            B.withByteArray input $ \inpPtr -> do
                c_blake2b_256_personalized
                       perPtr (BS.length personalization)
                       inpPtr (BS.length input) outPtr
                  >>= \case 0 -> pure ()
                            r -> error $ "blake2b returned with " ++ show r


foreign import ccall unsafe "doDelayThingy" c_doDelayThingy :: Int -> IO Int
foreign import ccall unsafe "getSleeps" c_getSleeps :: IO Int
foreign import ccall unsafe "getWakes" c_getWakes :: IO Int


doDelayThingy :: Int -> Int
doDelayThingy = unsafePerformIO . c_doDelayThingy


doDelayThingyManyThreads :: IO ()
doDelayThingyManyThreads = do

  lock1 <- newEmptyTMVarIO
  lock2 <- newEmptyTMVarIO
  count <- newTVarIO 0

  let
    f i = do
      join $ atomically do
        if mod i 2 == 0
           then putTMVar lock1 ()
           else putTMVar lock2 ()
        let r = doDelayThingy i
        if mod r 2 == 0
           then takeTMVar lock1
           else takeTMVar lock2
        if r /= i
          then pure $ Prelude.putStrLn "fault"
          else do
            c <- readTVar count
            writeTVar count (c+1)
            pure $ pure ()

  let n = 100
  mapM_ (forkIO . f) [1..n]

  atomically do
    c <- readTVar count
    checkSTM $ c == n

  print =<< c_getSleeps
  print =<< c_getWakes
