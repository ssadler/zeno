
module Zeno.CLI.Utils where

import Network.Komodo
import Network.Bitcoin
import Network.Ethereum
import Options.Applicative

import Zeno.Prelude


fromWifMethod :: Parser (IO ())
fromWifMethod =
  f <$> strArgument ( metavar "WIF" )
  where
  f wif = runFromSec $ either error id $ parseWif komodo wif

fromSecMethod :: Parser (IO ())
fromSecMethod = runFromSec <$> strArgument ( metavar "KEY" )

fromPubMethod :: Parser (IO ())
fromPubMethod = runFromPub <$> strArgument ( metavar "PUB" )

runFromSec :: SecKey -> IO ()
runFromSec sk = do
  pk <- derivePubKeyIO sk
  putStrLn $ printf "SecKey:    %s" (stripQuotes $ show sk)
  putStrLn $ printf "PubKey:    %s" (stripQuotes $ show pk)
  runFromPub pk

runFromPub :: PubKey -> IO ()
runFromPub pk = do
  kmdAddress <- deriveKomodoAddress pk
  ethAddress <- deriveEthAddress pk
  putStrLn $ printf "KMD addr:  %s" (show kmdAddress)
  putStrLn $ printf "ETH addr:  %s" (show ethAddress)

stripQuotes :: String -> String
stripQuotes s = do
  drop 1 $ take (length s - 1) s
