
module Zeno.Notariser.Shuffle where

import Control.Monad
import Data.Bits
import qualified Data.Map as Map
import Data.Word
import Data.ByteString.Short (unpack)
import Data.FixedBytes
import Network.Bitcoin (sha256b)
import Zeno.Consensus


-- | Shuffle a list using the round seed
roundShuffle :: [a] -> Consensus [a]
roundShuffle items = do
  when (length (take 0x10000 items) == 0x10000) do
    error "distribute: items too long"

  shuffleWithWords items . infWord16 . infBytes <$> getRoundSeed


-- | Get high bit from a number
hibit :: (Bits n, Num n) => n -> Int
hibit 0 = 0
hibit i = 1 + hibit (shiftR i 1)


-- | Convert infinite series of bytes to words
infWord16 :: [Word8] -> [Word16]
infWord16 = f . map fromIntegral
  where f (a:b:xs) = shift a 0xff + b : f xs


-- | Generate an infinite series of bytes from hash
infBytes :: Bytes32 -> [Word8]
infBytes seed = f $ unpack $ unFixed seed
  where
  f (b:xs) = b : f xs
  f [] = infBytes $ sha256b $ fromFixed seed


-- List shuffle that takes a random series of 16 bit words.  In order to select
-- a random element from the list, it skips random inputs that do not produce
-- indexes within range.  This means that we throw out up to half of the random
-- inputs, but the selection is unbiased.
shuffleWithWords :: [a] -> [Word16] -> [a]
shuffleWithWords [] _ = []
shuffleWithWords items (word:words) =
  let limit = length items - 1                        -- valid indexes are 0..n-1
      mask = 2 ^ hibit limit - 1                      -- mask for neccesary bits
      idx = fromIntegral $ word .&. mask              -- take neccesary bits from word
      (a, chosen:b) = splitAt idx items               -- split the items
      ok = chosen : shuffleWithWords (a ++ b) words   -- ok branch
      skip = shuffleWithWords items words             -- skip branch
   in if idx <= limit then ok else skip               -- ok if index within range
shuffleWithWords _ [] =
  error "shuffleWithWords has no more words"


-- For demonstration purposes -------------------------------------------------------

demoShuffle :: Ord a => [a] -> Int -> Map.Map a Int
demoShuffle items n =
  Map.fromListWith (+) $
    inner items n (drop 10 $ infWord16 $ infBytes minBound)
  where
  inner _ 0 _ = mempty
  inner items n words =
    let r = shuffleWithWords items words
        nextWords = drop (length items * 2) words
     in (head r, 1) : inner items (n-1) nextWords
