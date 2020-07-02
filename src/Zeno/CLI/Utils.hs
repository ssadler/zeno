
module Zeno.CLI.Utils where

import qualified Haskoin as H

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
  let pk = derivePubKey sk
  putStrLn $ printf "SecKey:    %s" (stripQuotes $ show sk)
  putStrLn $ printf "PubKey:    %s" (stripQuotes $ show pk)
  let hsk = read $ show $ show sk
  putStrLn $ printf "KMD Wif:   %s" (stripQuotes $ toS $ H.toWif komodo $ H.SecKeyI hsk True)

  runFromPub pk

runFromPub :: PubKey -> IO ()
runFromPub pk = do
  let kmdAddress = deriveKomodoAddress pk
  let ethAddress = deriveEthAddress pk
  putStrLn $ printf "KMD addr:  %s" (show kmdAddress)
  putStrLn $ printf "ETH addr:  %s" (show ethAddress)

stripQuotes :: String -> String
stripQuotes = filter (/='"')
