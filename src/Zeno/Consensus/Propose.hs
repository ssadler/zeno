
module Zeno.Consensus.Propose
  ( propose
  , proposeWithAction
  ) where

import Network.Ethereum.Crypto
import Data.Serialize
import qualified Data.ByteString as BS
import qualified Data.Map as Map

import Zeno.Prelude
import Zeno.Consensus.Round
import Zeno.Consensus.Types


-- | So whats the deal with the proposers. We need a proposer sometimes to
--   provide some arbitrary order for some pieces of verifiable data.
--   On the surface it's tricky because you're depending on a single member,
--   but with fallbacks we can make the process more robust, in case a
--   proposer can't produce a response in time. Additionally, a proposer
--   timeout is a good metric of performance, ie, it verifies that they
--   are online and verifying chains, and it's not very sensitive to latency,
--   so a proposer in New Zealand isn't disadvantaged as compared to one
--   in New York.
--
--   However, if we are going to use proposer timeouts as a metric of
--   performance, it should be fair, ie, select each member an equal number
--   of times. A pseudorandom selection should do this over a long enough
--   period of time, but will always be subject to debate as to weather or
--   not it was actually fair for a given period of time (the so called "law"
--   of averages). And in the short term it will just cause anxiety and
--   confusion. However, by taking advantage of a monotonically increasing
--   sequence that we can just nab from somewhere, we can provide a fair round
--   robin, that is sensitive only to changes in the list of members.
--
--   A chosen proposer will still have fallbacks so that the round can complete
--   even if one proposer times out. We will not record statistics for the
--   fallback proposers, because the fallback proposers will not be called upon
--   an equal number of times.
--
determineProposers :: Maybe Int -> Consensus [(Address, Bool)]
determineProposers (Just seq) = do
  ConsensusParams{members'}  <- asks has
  let primary = members' !! mod seq (length members')
  let fallbacks = filter ((/=primary) . fst) <$> determineProposers Nothing
  ((primary, True):) . take 2 <$> fallbacks
determineProposers Nothing = do
  ConsensusParams{members'}  <- asks has
  seed <- getStepSeed
  {- This gives fairly good distribution:
  import hashlib
  dist = [0] * 64
  for i in xrange(100000):
      m = hashlib.sha256(str(i))
      d = sum(map(ord, m.digest()))
      dist[d%64] += 1
  print dist
  -}
  let msg2sum = sum . map fromIntegral . BS.unpack . sha3' . encode
      i = mod (msg2sum seed) (length members')
  pure $ drop i $ (,False) <$> cycle members'


type CollectProposal i o = Address -> Collect (Maybe i) o

propose'
  :: forall i o. BallotData i
  => String -> Maybe Int -> CollectProposal i o -> Consensus i -> Consensus o
propose' name mseq collect mobj = do
  incMajorStepNum
  determineProposers mseq >>= go
    where
      go :: [(Address, Bool)] -> Consensus o
      go [] = throwIO ConsensusTimeout
      go ((pAddr, isPrimary):xs) = do

        let next = incMinorStepNum >> go xs

        obj <- do
          myAddress <- getMyAddress
          if pAddr == myAddress
             then logDebug ("Proposer is: %s (me)" % show pAddr) >> (Just <$> mobj)
             else logDebug ("Proposer is: %s" % show pAddr) >> pure Nothing
        
        catches

          do step' (printf "propose %s" name) (collect pAddr) obj

          [ Handler $
              \ConsensusTimeout -> do
                logInfo $ "Proposer timed out: " ++ show pAddr
                incMinorStepNum
                next

          , Handler $
              \(ConsensusInvalidProposal s) -> do
                logWarn $ "Invalid proposal from: " ++ show pAddr
                logWarn s
                next
          ]


propose :: BallotData i => String -> Maybe Int -> Consensus i -> Consensus (Ballot i)
propose name mseq mobj = do
  proposeWithAction name mseq mobj pure


proposeWithAction
  :: BallotData a
  => String -> Maybe Int -> Consensus a -> (Ballot a -> Zeno ConsensusContext o) -> Consensus o
proposeWithAction name mseq mobj act =
  propose' name mseq collect mobj where
    collect pAddr inv =
      case Map.lookup pAddr inv of
        Just (s, Just obj2) -> Just <$> act (Ballot pAddr s obj2)
        Just (s, Nothing) -> do
          -- TODO: handle mischief
          logDebug $ "Mischief: %s sent empty proposal" % (show pAddr)
          throwIO ConsensusTimeout
        Nothing -> pure Nothing

