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
       --    (command "tx"       $ infoH txMethods         $ progDesc "Tx methods")
       -- <> (command "keypair"  $ infoH keyPairMethod     $ progDesc "Generate a priv/pub key pair")
       -- <> (command "contract" $ infoH contractMethods   $ progDesc "Generate contracts")
       -- <> (command "prove"    $ infoH provingMethods    $ progDesc "Generate proofs")
-- 
--     txMethods = subparser $
--            (command "encode"    $ infoH encodeTxMethod    $ progDesc "Encode a json transaction")
--         <> (command "sign"      $ infoH signTxMethod      $ progDesc "Sign a transaction on stdin")
--         <> (command "decode"    $ infoH decodeTxMethod    $ progDesc "Decode a transaction on stdin")
--         <> (command "from"      $ infoH recoverFromMethod $ progDesc "Recover address")
--         <> (command "txid"      $ infoH txidMethod        $ progDesc "Get transaction id")
-- 
--     contractMethods = subparser $
--            (command "contractProxy" $ infoH contractProxyMethod $ progDesc "Get code for proxy contract")
-- 
     notariserMethods = subparser $
            (command "kmdeth" $ infoH runEthNotariserMethod  $ progDesc "Run KMD -> ETH notariser")
         <> (command "seed"   $ infoH runSeedNotariserMethod $ progDesc "Run notariser seed node")
-- 
--     provingMethods = subparser $
--            (command "ethkmd" $ infoH proveEthKmdTransactionMethod $ progDesc "Prove ETH transaction on KMD")


-- encodeTxMethod :: Parser Method
-- encodeTxMethod =
--   let act ma = do
--         tx <- ma 
--         BS8.putStrLn $ ("0x"<>) $ toHex $ encodeTx tx
--    in act <$> optJsonOrStdin (metavar "JSON TX")
-- 
-- signTxMethod :: Parser Method
-- signTxMethod = act <$> argument skArg (metavar "secret key")
--   where skArg = maybeReader $ secKey . fst . B16.decode . fromString
--         act sk = do
--           txBin <- fromHex <$> BS8.getLine
--           let tx = rlpDecode $ rlpDeserialize txBin
--               signed = signTx tx sk
--           BS8.putStrLn $ ("0x"<>) $ toHex $ encodeTx signed
-- 
-- 
-- decodeTxMethod :: Parser Method
-- decodeTxMethod = jsonMethod $ pure $ do
--   txBin <- fromHex <$> BS8.getLine
--   let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
--   pure $ toJSON tx
-- 
-- 
-- recoverFromMethod :: Parser Method
-- recoverFromMethod = jsonMethod $ pure $ do
--   (txBin,_) <- B16.decode <$> BS8.getContents
--   let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
--       toObj pk = object [ "pub" .= toJsonHex (BS.drop 1 (exportPubKey False pk))
--                         , "addr" .= pubKeyAddr pk
--                         ]
--   maybe (fail "Invalid recovery data") (pure . toObj) $ recoverFrom tx
-- 
-- 
-- txidMethod :: Parser Method
-- txidMethod = pure $ do
--   (txBin,_) <- B16.decode <$> BS8.getContents
--   let tx = rlpDecode $ rlpDeserialize txBin :: Transaction
--   BS8.putStrLn $ toHex $ unSha3 $ txid tx
-- 
-- 
-- jsonMethod :: Parser (IO Value) -> Parser Method
-- jsonMethod m = e <$> doPretty <*> m
--   where doPretty = switch (long "pretty" <> short 'p' <> help "Pretty print output")
--         e p act = act >>= C8L.putStrLn . if p then encodePretty' o else encode
--         o = defConfig { confCompare = compare }
-- 
-- 
-- keyPairMethod :: Parser Method
-- keyPairMethod = jsonMethod $ pure $ do
--   sk <- genSecKey
--   let pk = derivePubKey sk
--   return $ object [ "secKey" .= Key sk
--                   , "pubKey" .= Key pk
--                   , "addr"   .= pubKeyAddr pk
--                   ]
-- 
-- 
-- contractProxyMethod :: Parser Method
-- contractProxyMethod = act <$> optInit <*> argAddress
--   where act doInit addr =
--           let code = if doInit then initEvmContract $ contractProxy addr
--                                else contractProxyCode addr
--            in putStrLn code
--         argAddress = argument auto (metavar "target contract address")
--         optInit = switch $ long "init" <> short 'i' <> help "Return code for contract init"
-- 
-- 
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

-- 
-- proveEthKmdTransactionMethod :: Parser Method
-- proveEthKmdTransactionMethod =
--   runProveEthKmdTransaction <$> optGethConfig <*> optKmdConfigPath <*> act
--   where
--     act = proveEthKmdTransaction <$> symbolArg <*> kmdTx <*> txidArg
--     symbolArg = strArgument $ metavar "SYMBOL" <> help "Target symbol"
--     kmdTx = strArgument     $ metavar "KMDTX" <> help "KMD import tx"
--     txidArg = optional $ argument auto $ metavar "TXID" <> help "Ethereum transaction ID"
-- 
