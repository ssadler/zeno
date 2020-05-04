
module Zeno.Notariser.KMDETH where

import Network.Bitcoin
import Network.Ethereum
import Network.Komodo

import Zeno.Notariser.UTXO

import Zeno.EthGateway
import Zeno.Notariser.KMD
import Zeno.Notariser.Types
import Zeno.Consensus
import Zeno.Config
import Zeno.Prelude
import Zeno.Prelude.Lifted


kmdInputAmount :: Word64
kmdInputAmount = 9800

consensusTimeout :: Int
consensusTimeout = 5 * 1000000



runNotariseKmdToEth :: GethConfig -> ConsensusNetworkConfig -> Address -> FilePath -> RAddress -> IO ()
runNotariseKmdToEth gethConfig consensusConfig gateway kmdConfPath kmdAddress = do
  --threadDelay 2000000
  bitcoinConf <- loadBitcoinConfig kmdConfPath
  wif <- runZeno bitcoinConf $ queryBitcoin "dumpprivkey" [kmdAddress]
  sk <- either error pure $ parseWif komodo wif

  withConsensusNode consensusConfig $
    \node -> do
      let notariser = EthNotariser bitcoinConf node gethConfig gateway sk
      runZeno notariser ethNotariser


ethNotariser :: Zeno EthNotariser ()
ethNotariser = do
  KomodoIdent{..} <- asks has
  logInfo $ "My KMD address: " ++ show kmdAddress
  (EthIdent _ ethAddr) <- asks has
  logInfo $ "My ETH address: " ++ show ethAddr

  forkMonitorUTXOs kmdInputAmount 5 50

  forever do -- here is the place to handle errors
    withNotariserConfig "KMDETH" $ do
      
      pure () :: Zeno NotariserConfig ()

      getLastNotarisationOnEth >>=

        \case
          Nothing -> do
            logDebug "No prior notarisations found"
            height <- getKmdProposeHeight 10
            notariseToETH height

          Just (lastHeight, _, _) -> do
            logDebug $ "Found prior notarisation at height %s" % lastHeight
            -- Check if backnotarised to KMD
            getLastNotarisation "ETHTEST" >>=

              \case
                Just (Notarisation _ _ nor@NOR{..}) | blockNumber == lastHeight -> do
                  let _ = nor :: NotarisationData Sha3
                  logDebug "Found backnotarisation, proceed with next notarisation"
                  newHeight <- getKmdProposeHeight 10
                  if newHeight > lastHeight
                     then notariseToETH newHeight
                     else do
                       logDebug "Not enough new blocks, sleeping 60 seconds"
                       threadDelay $ 60 * 1000000

                _ -> do
                  logDebug "Backnotarisation not found, proceed to backnotarise"
                  mutxo <- getKomodoUtxo kmdInputAmount
                  case mutxo of
                    Nothing -> do
                      logInfo "Waiting for UTXOs"
                      liftIO $ threadDelay $ 180 * 1000000
                    Just utxo -> do
                      notariseToKMD utxo lastHeight

  where
    withNotariserConfig :: Text -> Zeno NotariserConfig a -> Zeno EthNotariser a
    withNotariserConfig configName act = do
      gateway <- asks getEthGateway
      (threshold, members) <- ethCallABI gateway "getMembers()" ()
      JsonInABI v <- ethCallABI gateway "getConfig()" configName

      let (notarisationsContract, kmdAlias) =
            let e = error $ "gateway misconfigured for " % configName
             in maybe e id $ v .? "{notarisationsContract}"
      
          config notariser = NotariserConfig
                      { notarisationsContract
                      , members = Members members
                      , threshold
                      , notariser
                      , kmdAlias
                      }

      zenoReader config act
        


-- TODO: need error handling here with strategies for configuration errors, member mischief etc.
notariseToETH :: Word32 -> Zeno NotariserConfig ()
notariseToETH height32 = do
  NotariserConfig{..} <- ask

  let height = fromIntegral height32
  logDebug $ "Notarising from block %i" % height

  ident <- asks has
  let gateway = getEthGateway notariser
  let cparams = ConsensusParams (unMembers members) ident consensusTimeout
  r <- ask
  let run = liftIO . runZeno r


  -- we already have all the data for the call to set the new block height
  -- in our ethereum contract. so create the call.

  blockHash <- bytes . unHex <$> queryBitcoin "getblockhash" [height]
  let notariseCallData = abi "notarise(uint256,bytes32,bytes)"
                             (height, blockHash :: Bytes 32, "" :: ByteString)
      proxyParams = (notarisationsContract, height, notariseCallData);
      sighash = ethMakeProxySigMessage proxyParams

  -- Ok now we have all the parameters together, we need to collect sigs and get the tx

  tx <- runConsensus cparams proxyParams $ do
    {- The trick is, that during this whole block inside runConsensus,
       each step will stay open until the end so that lagging nodes can
       join in late. -}

    run $ logDebug "Step 1: Collect sigs"
    sigBallots <- stepWithTopic sighash (collectThreshold threshold) ()

    run $ logDebug "Step 2: Get proposed transaction"
    let proxyCallData = ethMakeProxyCallData proxyParams (bSig <$> unInventory sigBallots)
    txProposed <- propose $ run $ ethMakeTransaction gateway proxyCallData
    -- TODO: verifications on proposed tx

    run $ logDebug "Step 3: Confirm proposal"
    _ <- step collectMajority ()
    pure txProposed

  logDebug "Step 4: Submit transaction"
  receipt <- postTransactionSync tx
  logDebug $ "posted tranaction: " ++ show receipt
  pure ()


getLastNotarisationOnEth :: (Integral i, Has NotariserConfig r, Has GethConfig r)
                         => Zeno r (Maybe (i, Bytes 32, ByteString))
getLastNotarisationOnEth = do
  NotariserConfig{..} <- asks has
  r <- ethCallABI notarisationsContract "getLastNotarisation()" ()
  pure $
    case r of
      (0::Integer, _, _) -> Nothing
      (h, hash, extra) -> Just (fromIntegral h, hash, extra)
