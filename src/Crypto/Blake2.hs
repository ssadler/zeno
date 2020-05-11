
module Crypto.Blake2 where


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


toHex = B16.encode

runTestBlake2Personalized p b e = do
  let r = B16.encode $ blake2bPersonalized p b
  if r == e
     then BS.putStrLn "OK"
     else BS.putStrLn $ r <> " /= " <> e


testBlake2Personalized = do
  runTestBlake2Personalized "" "" "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
  runTestBlake2Personalized "" "Hello World" "1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00"
  runTestBlake2Personalized "ZcashPrevoutHash" "" "d53a633bbecf82fe9e9484d8a0e727c73bb9e68c96e72dec30144f6a84afa136"
