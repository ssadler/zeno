{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}

module Main where

import           Options.Applicative

import           Zeno.Config
import           Zeno.Consensus.P2P (runSeed)
import           Zeno.Notariser.KMDETH
import           Zeno.Prelude


main :: IO ()
main = do
  join $ customExecParser (prefs showHelpOnEmpty) parseAct

type Method = IO ()

parseAct :: ParserInfo Method
parseAct = infoH topMethods $ fullDesc <> progDesc "Notariser for Komodo network"
   where
     infoH m = info $ m <**> helper

     topMethods = subparser $
          (command "notarise" $ infoH notariserMethods  $ progDesc "Notariser modes")
       <> (command "notarize" $ infoH notariserMethods  $ progDesc "see: (notarise)")
     
     notariserMethods = subparser $
            (command "kmdeth" $ infoH runEthNotariserMethod  $ progDesc "Run KMD -> ETH notariser")
         <> (command "seed"   $ infoH runSeedNotariserMethod $ progDesc "Run notariser seed node")


runEthNotariserMethod :: Parser Method
runEthNotariserMethod =
  runNotariseKmdToEth
  <$> optGethConfig
  <*> optConsensusConfig
  <*> optGateway
  <*> optKmdConfigPath
  <*> strOption ( long "address" <> help "kmd address" <> metavar "KMD" )


runSeedNotariserMethod :: Parser Method
runSeedNotariserMethod = runSeed <$> optHost <*> optPort
