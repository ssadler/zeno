
module TestStep where

import TestUtils

import Control.Monad.Except
import Crypto.Secp256k1Wrapped
import qualified Data.Map.Strict as Map

import Zeno.Consensus.Step
import Zeno.Consensus.Types
import Zeno.Prelude


unit_merge_inventory :: IO ()
unit_merge_inventory = do
  sig <- sign sk (minBound :: Bytes32)
  let theirs = Map.fromList [(minBound, (sig, ())), (maxBound, (sig, ()))]

  let r = mergeInventory mempty mempty theirs
  r @?= Left "Got ballot data for non member"

  let r = mergeInventory (Map.keysSet theirs) mempty theirs
  r @?= Left "Got ballot data with invalid signature"
  
  pure ()

sk :: SecKey
sk = "afdf0310c3feab2378267034604525c6adbca360630e187ae79eb534698145cd"
