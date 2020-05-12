
module Crypto.Blake2
  ( blake2bPersonalized
  ) where


import qualified Data.ByteString.Base16 as B16
import Data.ByteArray as B
import Data.ByteString as BS
import Data.Word
import Foreign.Ptr (Ptr)
import System.IO.Unsafe


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

