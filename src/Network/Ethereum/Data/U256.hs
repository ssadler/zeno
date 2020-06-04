{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Data.U256 where


import Data.Aeson
import Data.Char
import Data.RLP



newtype U256 = U256 { unU256 :: Integer }
  deriving (Eq, Ord, Enum, Num, Integral, Real, RLPEncodable)

instance FromJSON U256 where
  {-# INLINABLE parseJSON #-}
  parseJSON v = do
    (pre, body) <- splitAt 2 <$> parseJSON v
    r <- foldl un 0 <$> mapM toDec body
    if pre == "0x" then pure $ U256 r else fail "Invalid hex prefix"
    where
    un :: Integer -> Integer -> Integer
    un a b = a * 16 + (fromIntegral b :: Integer)
    toDec c = maybe (fail "Invalid hex char") pure $ lookup c $ zip "0123456789abcdef" [0..]

instance ToJSON U256 where
  toJSON (U256 n) =
    let hexChar i = chr $ i + if i < 10 then 48 else 87
        hex 0 s = "0x" <> s
        hex i s = hex d $ c : s
          where (d,q) = divMod i 16
                c = hexChar $ fromIntegral q
     in toJSON $ hex n $ if n == 0 then "0" else ""
  {-# INLINABLE toJSON #-}

instance Show U256 where
  show (U256 i) = show i
