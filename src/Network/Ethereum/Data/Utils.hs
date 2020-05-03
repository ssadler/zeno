
module Network.Ethereum.Data.Utils where


import           Data.Bits
import           Data.Word

import qualified Data.ByteString as BS


packInteger :: Integer -> BS.ByteString
packInteger = BS.pack . integerToBytes

integerToBytes :: (Bits a, Integral a) => a -> [Word8]
integerToBytes = reverse . pack
  where
    pack 0 = []
    pack x = fromIntegral (x .&. 255) : pack (x `shiftR` 8)

unpackInteger :: BS.ByteString -> Integer
unpackInteger = unpack . BS.unpack
  where
    unpack [] = 0
    unpack (byte:rest) = fromIntegral byte `shift` (8 * length rest) + unpack rest
