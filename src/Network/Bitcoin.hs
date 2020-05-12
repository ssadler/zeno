{-# LANGUAGE FlexibleInstances #-}

module Network.Bitcoin where

import qualified Data.Binary as Bin
import qualified Data.Serialize as Ser

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Attoparsec.ByteString.Char8 as A

import           Zeno.Data.Aeson hiding (Parser)
import           Zeno.Prelude
import           Zeno.Prelude.Lifted

import qualified Haskoin as H
import           Network.HTTP.Simple
import           Network.JsonRpc
import           Network.ZCash.Sapling


data BitcoinConfig =
  BitcoinConfig
    { getUser :: ByteString
    , getPassword :: ByteString
    , getHost :: ByteString
    , getPort :: Int
    } deriving (Show)


loadBitcoinConfig :: FilePath -> IO BitcoinConfig
loadBitcoinConfig path = do
  runZeno () $ logInfo $ "Loading bitcoin config: " ++ path
  configData <- liftIO $ expandPath path >>= BS.readFile
  let p p1 p2 = parseOnly (parseItem p1 p2) configData <|> Left p1
  let econfig =
        BitcoinConfig <$>
          (p "rpcuser"     toEnd   <|> pure "")          <*>
          (p "rpcpassword" toEnd   <|> pure "")          <*>
          (p "rpchost"     toEnd   <|> pure "127.0.0.1") <*>
          (p "rpcport"     decimal <|> pure 7771)

  case econfig of
    Left e -> error $ "Could not parse variable: " ++ e ++ " from config: " ++ path
    Right c -> pure c
  where
  toEnd = takeTill (inClass " \n")
  parseItem name parseVal = do
    let user = matchName >> skipSpace >> "=" >> skipSpace >> parseVal
        skipLine = takeTill (=='\n') >> endOfLine
        matchName = A.string $ fromString name
    user <|> (skipLine >> parseItem name parseVal)

queryBitcoin :: (Has BitcoinConfig r, FromJSON a, ToJSON b) => Text -> b -> Zeno r a
queryBitcoin method params = hasReader $ do
  (BitcoinConfig user pass host port) <- ask
  let r = fromString $ "http://" ++ BS8.unpack host ++ "/"
  let endpoint =
        HttpEndpoint $
          setRequestBasicAuth user pass $ 
          setRequestPort port r
  queryJsonRpc endpoint method params

bitcoinSubmitTxSync :: Has BitcoinConfig r => Int -> SaplingTx -> Zeno r H.TxHash
bitcoinSubmitTxSync confirmations tx = do
  txid <- queryBitcoin "sendrawtransaction" [tx]
  fix $ \f -> do
    threadDelay 5000000
    rawtx <- queryBitcoin "getrawtransaction" (txid, 1::Int)
    case rawtx .? "{confirmations}" of
         Nothing -> f
         Just (n::Int) -> if n < confirmations then f else pure txid

bitcoinGetHeight :: Has BitcoinConfig r => Zeno r Word32
bitcoinGetHeight = queryBitcoin "getinfo" () <&> (.!"{blocks}")


parseWif :: H.Network -> Text -> Either String H.SecKey
parseWif net wif = do
  case H.fromWif net wif of
    Just (H.SecKeyI seckey True) -> pure seckey
    _ -> Left $ "Couldn't parse WIF from daemon using network " ++ H.getNetworkName net


-- Instances ------------------------------------------------------------------

instance Bin.Binary H.TxIn where
  put = Bin.put . Ser.encode
  get = Bin.get >>= either fail pure . Ser.decode

instance Bin.Binary H.OutPoint where
  put = Bin.put . Ser.encode
  get = Bin.get >>= either fail pure . Ser.decode

instance Bin.Binary H.PubKeyI where
  put = Bin.put . Ser.encode
  get = Bin.get >>= either fail pure . Ser.decode

