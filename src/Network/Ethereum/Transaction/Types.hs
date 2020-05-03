{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Ethereum.Transaction.Types where

import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as BS8
import           Data.RLP as RLP
import GHC.Generics

import           Network.Ethereum.Crypto
import           Network.Ethereum.Data.Utils
import           Zeno.Data.Aeson
import           Zeno.Prelude


data Transaction = Tx
  { _nonce    :: Integer
  , _value    :: Integer
  , _to       :: Maybe Address
  , _sig      :: Maybe CompactRecSig
  , _gasPrice :: Integer
  , _gas      :: Integer
  , _data     :: ByteString
  , _chainId  :: ChainId
  } deriving (Eq, Show, Generic)

instance Bin.Binary Transaction


instance RLPEncodable Transaction where
  rlpEncode tx =
    let body =
          [ rlpEncode $ _nonce tx
          , rlpEncode $ _gasPrice tx
          , rlpEncode $ _gas tx
          , rlpEncode $ maybe "" fromAddress $ _to tx
          , rlpEncode $ _value tx
          , rlpEncode $ _data tx
          ]

        encodeSig (CompactRecSig r s v) =
          [ rlpEncode $ encodeSpecialV (_chainId tx) v
          , rlpEncode $ unpackInteger $ fromShort r
          , rlpEncode $ unpackInteger $ fromShort s
          ]

        noSig = [rlpEncode $ _chainId tx, RLP.String "", RLP.String ""]

   in RLP.Array $ body ++ maybe noSig encodeSig (_sig tx)

  rlpDecode rlp@(RLP.Array [_,_,_,_,_,_]) = do
    (n,gp,g,to,val,d) <- rlpDecode rlp
    mto <- case BS8.length to of
                   20 -> pure $ Just (Address to)
                   0  -> pure $ Nothing
                   _  -> Left "Invalid address"
    pure $ Tx n val mto Nothing gp g d 0

  rlpDecode (RLP.Array [n,gp,g,to,val,d,_v,_r,_s]) = do
    tx <- rlpDecode $ RLP.Array [n,gp,g,to,val,d]

    let pad32 "" = ""
        pad32 bs = if BS8.length bs < 32 then pad32 ("\0" <> bs) else bs

    r <- toShort . pad32 <$> rlpDecode _r
    s <- toShort . pad32 <$> rlpDecode _s

    sv <- rlpDecode _v
    let (c, v) = decodeSpecialV sv
        crs = CompactRecSig s r v

    pure $ 
      if r == "" && s == ""
         then tx { _sig = Nothing, _chainId = c }
         else tx { _sig = Just crs, _chainId = c }

  rlpDecode o = error $ "Invalid RLP Transaction: " ++ show o


newtype ChainId = ChainId { unChainId :: Word8 }
  deriving (Show, Num, Eq, Generic, ToJSON, FromJSON, RLPEncodable)

instance Bin.Binary ChainId

encodeSpecialV :: ChainId -> Word8 -> Word8
encodeSpecialV 1 v = (v + 27)
encodeSpecialV (ChainId c) v = v + c * 2 + 35

decodeSpecialV :: Word8 -> (ChainId, Word8)
decodeSpecialV 27 = (1, 1)
decodeSpecialV 28 = (1, 1)
decodeSpecialV v' = (ChainId $ mod (v'+1) 2, quot (v' - 35) 2)


instance ToJSON Transaction where
  toJSON tx =
    object [ "nonce"    .= _nonce tx
           , "value"    .= _value tx
           , "to"       .= _to tx
           , "sig"      .= _sig tx
           , "gasPrice" .= _gasPrice tx
           , "gas"      .= _gas tx
           , "data"     .= toJsonHex (_data tx)
           , "chainId"  .= _chainId tx
           ]


instance FromJSON Transaction where
  parseJSON = withStrictObject "Transaction" $ \o -> do
    Tx <$> o .:-  "nonce"
       <*> o .:-  "value"
       <*> o .:-? "to"
       <*> o .:-? "sig"
       <*> o .:-  "gasPrice"
       <*> o .:-  "gas"
       <*> (o .:-? "data" >>= fromJsonHex . maybe "" id)
       <*> o .:-  "chainId"
