
module TestBlake2 where

import TestUtils
import qualified Data.ByteString.Base16 as B16
import Crypto.Blake2
import Data.ByteString


runTestBlake2Personalized :: ByteString -> ByteString -> ByteString -> IO ()
runTestBlake2Personalized p b e = do
  let r = B16.encode $ blake2bPersonalized p b
  r @?= e

unit_1 = runTestBlake2Personalized "" ""
          "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"

unit_2 = runTestBlake2Personalized "" "Hello World"
          "1dc01772ee0171f5f614c673e3c7fa1107a8cf727bdf5a6dadb379e93c0d1d00"

unit_3 = runTestBlake2Personalized "ZcashPrevoutHash" ""
          "d53a633bbecf82fe9e9484d8a0e727c73bb9e68c96e72dec30144f6a84afa136"
