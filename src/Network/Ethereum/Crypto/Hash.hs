
module Network.Ethereum.Crypto.Hash
  ( Sha3(..)
  , sha3
  , sha3'
  , sha3b
  ) where

import           Crypto.Hash
import           Data.ByteString.Short
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.FixedBytes


type Sha3 = Bytes32


sha3' :: ByteString -> ByteString
sha3' bs = BS.pack (BA.unpack (hash bs :: Digest Keccak_256))

sha3 :: ByteString -> Sha3
sha3 = toFixed . sha3'

sha3b :: ByteString -> Bytes32
sha3b bs = unsafeToFixed $ pack (BA.unpack (hash bs :: Digest Keccak_256))

