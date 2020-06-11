
module Zeno.Consensus.Propose
  ( propose
  ) where

import Network.Ethereum.Crypto.Address
import Network.Ethereum.Crypto.Hash
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
determineProposers :: Maybe ProposerSequence -> Consensus [(Address, Bool)]
determineProposers (Just (ProposerSequence seq)) = do
  ConsensusParams{members'}  <- asks has
  let primary = members' !! mod seq (length members')
  ((primary, True):) . take 2 <$> determineProposers Nothing
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

dispatchProposerTimeout :: Address -> Consensus ()
dispatchProposerTimeout proposer = do
  timeout <- ProposerTimeout proposer <$> getRoundId <*> getStepNum
  ConsensusParams{..} <- asks has
  let act = maybe mempty id onProposerTimeout'
  liftIO $ act timeout



propose :: forall a. BallotData a
        => String
        -> Maybe ProposerSequence
        -> Consensus a
        -> Consensus (Ballot a)
propose name mseq mobj = do
  incMajorStepNum
  determineProposers mseq >>= go
    where
      go :: [(Address, Bool)] -> Consensus (Ballot a)
      go [] = throwIO $ ConsensusTimeout "Ran out of proposers"
      go ((pAddr, isPrimary):xs) = do

        obj <- do
          myAddress <- getMyAddress
          if pAddr == myAddress
             then logDebug ("Proposer is: %s (me)" % show pAddr) >> (Just <$> mobj)
             else logDebug ("Proposer is: %s" % show pAddr) >> pure Nothing

        let
          collect inv = do
            case Map.lookup pAddr inv of
              Just (s, Just obj2) -> pure $ Just $ Ballot pAddr s obj2
              Just (s, Nothing) -> do
                -- TODO: handle mischief
                throwIO $ ConsensusTimeout ""
              Nothing -> pure Nothing
        
        catch
          do step' (printf "propose %s" name) collect obj

          \(ConsensusTimeout _) -> do
            logInfo $ "Timeout collecting for proposer: " ++ show pAddr
            when isPrimary do
              dispatchProposerTimeout pAddr
            incMinorStepNum
            go xs


