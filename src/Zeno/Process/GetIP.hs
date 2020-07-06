
module Zeno.Process.GetIP where

import Control.Monad.IO.Class
import Data.Bits
import qualified Data.Attoparsec.ByteString.Char8 as A
import Network.HTTP.Simple
import Network.Socket (HostAddress, hostAddressToTuple)
import Zeno.Prelude

getMyIpFromICanHazIp :: Zeno r HostAddress
getMyIpFromICanHazIp = do
  ipBs <- liftIO $ getResponseBody <$> httpBS "http://ipv4.icanhazip.com"
  let bail _ = fail $ "Could not parse IP from icanhazip.com: " <> show ipBs
  either bail pure $ A.parseOnly parseIp ipBs
  where
  oct i = do
    n <- A.decimal
    if n > (255 :: Integer)
       then fail "Invalid IP data"
       else pure $ fromIntegral $ shift n i

  parseIp = do
    let parts =
          [ oct  0 <* "."
          , oct  8 <* "."
          , oct 16 <* "."
          , oct 24 <* A.skipSpace <* A.endOfInput
          ]
    sum <$> sequence parts


renderIp :: HostAddress -> String
renderIp ip = "%i.%i.%i.%i" % hostAddressToTuple ip
