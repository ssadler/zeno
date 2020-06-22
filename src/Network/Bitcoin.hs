{-# LANGUAGE FlexibleInstances #-}

module Network.Bitcoin where

import           Crypto.Hash
import           Crypto.Secp256k1.Recoverable
import qualified Data.Serialize as Ser
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.ByteString.Short (pack)
import           Data.Attoparsec.ByteString.Char8 as A

import           Zeno.Data.Aeson hiding (Parser)
import           Zeno.Prelude

import qualified Haskoin as H
import           Network.HTTP.Simple
import           Network.JsonRpc
import           Network.ZCash.Sapling


newtype BitcoinConfig = BitcoinConfig { getRequest :: Request }
  deriving (Show)


-- TODO: parsing sucks, make a map
loadBitcoinConfig :: FilePath -> Zeno r BitcoinConfig
loadBitcoinConfig path = do
  logInfo $ "Loading bitcoin config: " ++ path
  configData <- liftIO $ expandPath path >>= BS.readFile
  let
    p :: String -> Parser a -> Either String a
    p p1 p2 = parseOnly (parseItem p1 p2) configData <|> Left p1
  let econfig =
        mkRequest <$>
          (p "rpcuser"     toEnd   <|> pure "")          <*>
          (p "rpcpassword" toEnd   <|> pure "")          <*>
          (p "rpchost"     toEnd   <|> pure "127.0.0.1") <*>
          (p "rpcport"     decimal <|> pure (7771 :: Int))

  case econfig of
    Left e -> error $ "Could not parse variable: " ++ e ++ " from config: " ++ path
    Right c -> pure $ BitcoinConfig c
  where
  toEnd = takeTill (inClass " \n")
  parseItem name parseVal = do

    let user = matchName >> skipSpace >> "=" >> skipSpace >> parseVal
        skipLine = takeTill (=='\n') >> endOfLine
        matchName = A.string $ fromString name
    user <|> (skipLine >> parseItem name parseVal)
  mkRequest user pass host port =
     setRequestBasicAuth user pass $ 
     setRequestPort (port :: Int) $
     fromString $ "http://" ++ toS host ++ "/"


queryBitcoin :: (Has BitcoinConfig r, FromJSON a, JsonRPCArgs b) => Text -> b -> Zeno r a
queryBitcoin method params = hasReader $ do
  endpoint <- asks getRequest
  queryJsonRpc (HttpEndpoint endpoint) method params

bitcoinSubmitTxSync :: Has BitcoinConfig r => Int -> SaplingTx -> Zeno r H.TxHash
bitcoinSubmitTxSync confirmations tx = do
  txid <- queryBitcoin "sendrawtransaction" [tx]
  fix $ \f -> do
    threadDelay 5000000
    rawtx <- queryBitcoin "getrawtransaction" (txid, 1::Int)
    case rawtx .? "{confirmations}" of
         Nothing -> f
         Just (n::Int) -> if n < confirmations then f else pure txid


bitcoinGetTxHeight :: Has BitcoinConfig r => H.TxHash -> Zeno r (Maybe Word32)
bitcoinGetTxHeight txHash = do
  queryBitcoin "getrawtransaction" (txHash, 1 :: Int) <&>
    (.? "{height}")

bitcoinGetHeight :: Has BitcoinConfig r => Zeno r Word32
bitcoinGetHeight = do
  queryBitcoin "getinfo" () <&> (.!"{blocks}")

parseWif :: H.Network -> Text -> Either String SecKey
parseWif net wif = toFixed. H.getSecKey <$> parseWifH net wif

parseWifH :: H.Network -> Text -> Either String H.SecKey
parseWifH net wif = do
  case H.fromWif net wif of
    Just (H.SecKeyI seckey True) -> pure seckey
    _ -> Left $ "Couldn't parse WIF from daemon using network " ++ H.getNetworkName net

sha256b :: BS.ByteString -> Bytes32
sha256b bs = unsafeToFixed $ pack (BA.unpack (hash bs :: Digest SHA256))
