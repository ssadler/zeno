{-# LANGUAGE OverloadedStrings #-}

module Network.Ethereum.Crypto.Trie
  ( Trie(..)
  , Nibbles
  , hexMapToTrie
  , mapToTrie
  , trieMapToTrie
  , orderedTrie
  , orderedTrieKey
  , trieProof
  , execTrieProof
  , hashNode
  , trieRoot
  , toNibbles
  , hexPrefixDecode
  , hexPrefixEncode
  ) where

import qualified Data.ByteString as BS

import           Network.Ethereum.Crypto.Hash
import           Data.RLP

import           Zeno.Prelude
import Debug.Trace


type Nibbles = [Word8]
data Trie = Branch16 [Trie] ByteString
          | Prefix Nibbles Trie
          | Leaf Nibbles ByteString
          | HashNode Sha3
          | N
          deriving (Eq, Show, Read)


-- Encoding -------------------------------------------------------------------
--
instance RLPEncodable Trie where
  rlpEncode = encodeTrie id
  rlpDecode = decodeTrie

encodeTrie :: (Trie -> Trie) -> Trie -> RLPObject
encodeTrie aux (Branch16 tries leaf) =
  Array $ map (encodeTrie aux . aux) tries ++ [rlpEncode leaf]
encodeTrie aux (Leaf k bs) =
  Array [rlpEncode $ hexPrefixEncode k True, rlpEncode bs]
encodeTrie aux (Prefix k t) =
  Array [rlpEncode $ hexPrefixEncode k False, encodeTrie aux $ aux t]
encodeTrie _ (HashNode sha) = rlpEncode sha
encodeTrie _ N = String ""

decodeTrie :: RLPObject -> Either String Trie
decodeTrie (Array [pref,v]) = do
  (isLeaf, k) <- hexPrefixDecode <$> rlpDecode pref
  if isLeaf then Leaf k <$> rlpDecode v
            else Prefix k <$> rlpDecode v
decodeTrie (Array items) | length items == 17 =
  let (branches, [val]) = splitAt 16 items
   in Branch16 <$> (mapM rlpDecode branches) <*> rlpDecode val
decodeTrie (String "") = pure N
decodeTrie (String bs) | BS.length bs == 32 = pure $ HashNode $ Sha3 bs

-- Crypto ---------------------------------------------------------------------
--
hashNode :: Trie -> Trie
hashNode N = N
hashNode n@(HashNode _) = n
hashNode t =
  let ser = rlpSerialize $ encodeTrie hashNode t
   in if BS.length ser < 32 then t else HashNode $ sha3 ser

trieRoot :: Trie -> Sha3
trieRoot = sha3 . rlpSerialize . encodeTrie hashNode

-- Sets a bytestring at a given node and encodes all the other nodes
-- The given bytestring will generally be "" when creating the proof,
-- and set to the original payload when verifying
trieProof :: Nibbles -> Trie -> Trie -> Trie
trieProof nibs (Leaf [] bs) (Leaf k _) | nibs == k = Leaf k bs
trieProof nibs newLeaf (Prefix k t) =
  let (a,b) = splitAt (length k) nibs
      r = Prefix k $ trieProof b newLeaf t
   in if k == a then r else error $ "Key mismatch: " ++ show (k,nibs)
trieProof [] (Leaf [] bs) (Branch16 tries _) = Branch16 (hashNode <$> tries) bs
trieProof (x:xs) newLeaf (Branch16 tries leaf) =
  let n i t = if i == x then trieProof xs newLeaf t else hashNode t
      nodes = zipWith n [0..] tries
   in Branch16 nodes leaf
trieProof k newLeaf t = error $
  "Cannot create proof with key: " ++ show k ++ " of: " ++ show t

execTrieProof :: Nibbles -> Trie -> Trie -> Sha3
execTrieProof n p t = trieRoot $ trieProof n p t

-- Creation -------------------------------------------------------------------
--
type HexMap = [(Nibbles, Trie)]

hexMapToTrie :: HexMap -> Trie
hexMapToTrie []       = N
hexMapToTrie [([],v)] = v
hexMapToTrie [(k,Leaf [] v)] = Leaf k v
hexMapToTrie [(k,v)]  = Prefix k v
hexMapToTrie xa       =
  let sharedPre = longestSharedPrefix $ fst <$> xa
      doPrefix = Prefix sharedPre $ hexMapToTrie $ trimMap (length sharedPre) xa
   in if null sharedPre then do16Node xa else doPrefix

do16Node :: HexMap -> Trie
do16Node xa@((k,v):xs) =
  if 0 == length k
     then Branch16 (do16NodeItems xs 0) (case v of Leaf [] bs -> bs)
     else Branch16 (do16NodeItems xa 0) ""

do16NodeItems :: HexMap -> Word8 -> [Trie]
do16NodeItems _     16 = []
do16NodeItems items i  =
  let part = takeWhile (\(k,_) -> take 1 k == [i]) items
      r = if null part then N else hexMapToTrie (trimMap 1 part)
   in r : do16NodeItems (drop (length part) items) (i+1)

trieMapToTrie :: [(ByteString, Trie)] -> Trie
trieMapToTrie pairs = hexMapToTrie $
  sortOn fst $ [(toNibbles k, v) | (k,v) <- pairs]

mapToTrie :: [(ByteString, ByteString)] -> Trie
mapToTrie pairs = trieMapToTrie [(k, Leaf [] v) | (k,v) <- pairs]

orderedTrie :: [ByteString] -> Trie
orderedTrie bss = mapToTrie $
  let packKey = rlpSerialize . rlpEncode
   in zip (packKey <$> [0::Integer ..]) bss

orderedTrieKey :: Integral a => a -> Nibbles
orderedTrieKey i = 
  let i' = fromIntegral i :: Integer
   in toNibbles $ rlpSerialize $ rlpEncode i'

-- Encoding -------------------------------------------------------------------
--
hexPrefixEncode :: Nibbles -> Bool -> ByteString
hexPrefixEncode s isLeaf =
  let rem = mod (length s) 2
      rest = fromNibbles $ drop rem s
      fst = (fromIntegral rem + if isLeaf then 2 else 0) * 16
             + if rem == 1 then s!!0 else 0
   in BS.cons fst rest

hexPrefixDecode :: ByteString -> (Bool, Nibbles)
hexPrefixDecode bs =
  let (fst, rest) = maybe (error "empty bs in hexPrefixDecode") id $ BS.uncons bs
      (d, q) = divMod fst 16
      nibs = toNibbles rest
      isLeaf = d >= 2
      nibs' = if mod d 2 == 1 then q : nibs else nibs
   in (isLeaf, nibs')

trimMap :: Int -> HexMap -> HexMap
trimMap i xs = [(drop i x, bs) | (x, bs) <- xs]

toNibbles :: ByteString -> Nibbles
toNibbles bs = BS.unpack bs >>=
  \b -> let (d,q) = divMod b 16 in [d,q]

fromNibbles :: Nibbles -> ByteString
fromNibbles [] = ""
fromNibbles (a:b:xs) = BS.cons (a*16+b) (fromNibbles xs)

longestSharedPrefix :: [Nibbles] -> Nibbles
longestSharedPrefix [] = []
longestSharedPrefix (x:xs) = foldr f x xs
  where f a b = fst <$> takeWhile (uncurry (==)) (zip a b)
