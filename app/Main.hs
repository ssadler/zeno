{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Options.Applicative

import           Zeno.Config
import           Zeno.Consensus
import           Zeno.Notariser.KMDETH
import           Zeno.Prelude
import           Zeno.CLI.Utils
import           Zeno.Version


main :: IO ()
main = join $ customExecParser (prefs showHelpOnEmpty) parseAct

type Method = IO ()

parseAct :: ParserInfo Method
parseAct = infoH topMethods $ fullDesc <> progDesc "Notariser for Komodo network"
   where
   infoH m = info $ m <**> helper

   topMethods = subparser $
        (command "notarize" $ infoH notariserMethods  $ progDesc "Notarizer modes")
     <> (command "notarise" $ infoH notariserMethods  $ mempty)
     <> (command "util"     $ infoH utilMethods       $ progDesc "Utilities")
     <> (command "version"  $ infoH showVersionMethod $ progDesc "Show version")
   
   notariserMethods = subparser $
        (command "kmdeth" $ infoH runEthNotariserMethod  $ progDesc "Run KMD -> ETH notariser")
     <> (command "seed"   $ infoH runSeedNotariserMethod $ progDesc "Run notariser seed node")

   utilMethods = subparser $
        (command "fromWif" $ infoH fromWifMethod $ progDesc "derive from WIF")
     <> (command "fromSec" $ infoH fromSecMethod $ progDesc "derive from secret")
     <> (command "fromPub" $ infoH fromPubMethod $ progDesc "derive from pubkey")


showVersionMethod :: Parser Method
showVersionMethod = pure $ putStrLn revisionInfo

withShowVersion :: Parser Method -> Parser Method
withShowVersion parse = (>>) <$> showVersionMethod <*> parse --fmap (putStrLn revisionInfo >>)


runEthNotariserMethod :: Parser Method
runEthNotariserMethod = withShowVersion $
  runNotariseKmdToEth
  <$> strOption ( long "pubkey" <> help "notariser pubkey" <> metavar "PUB" )
  <*> optGateway
  <*> optConsensusConfig
  <*> optGethConfig
  <*> optKmdConfigPath
  <*> optNoUI

runSeedNotariserMethod :: Parser Method
runSeedNotariserMethod = withShowVersion $ startSeedNode <$> optNetworkConfig <*> optNoUI
