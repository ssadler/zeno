
module Zeno.Notariser.KMDETH where

import Network.Bitcoin
import Network.Ethereum
import Network.Ethereum.Transaction
import Network.Komodo
import Network.HTTP.Simple
import Network.JsonRpc

import Zeno.Notariser.UTXO

import Zeno.EthGateway
import Zeno.Notariser.KMD
import Zeno.Notariser.Types
import Zeno.Consensus
import Zeno.Config
import Zeno.Prelude
import Zeno.Prelude.Lifted
import Zeno.Data.Aeson


consensusTimeout :: Int
consensusTimeout = 5 * 1000000


runNotariseKmdToEth :: GethConfig -> ConsensusNetworkConfig -> Address -> FilePath -> RAddress -> IO ()
runNotariseKmdToEth gethConfig consensusConfig gateway kmdConfPath kmdAddress = do
  threadDelay 1000000
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
  (EthIdent _ ethAddr) <- asks has
  logInfo $ "My KMD address: " ++ show kmdAddress
  logInfo $ "My ETH address: " ++ show ethAddr

  forkMonitorUTXOs kmdInputAmount 5 50

  runForever do

    nc@NotariserConfig{..} <- getNotariserConfig "KMDETH"
    asks has >>= checkConfig nc
    
    getLastNotarisationOnEth nc >>= 
      \case
        Nothing -> do
          logDebug "No prior notarisations found"
          height <- getKmdProposeHeight 10
          notariseToETH nc height

        Just ethnota@NOE{..} -> do
          logDebug $ "Found prior notarisation at height %i" % foreignHeight
          -- Check if backnotarised to KMD

          getLastNotarisation "ETHTEST" >>=
            \case
              Just (Notarisation _ _ nor@NOR{..}) | blockNumber == foreignHeight -> do
                logDebug "Found backnotarisation, proceed with next notarisation"
                newHeight <- getKmdProposeHeight 10
                if newHeight > foreignHeight
                   then notariseToETH nc newHeight
                   else do
                     logDebug "Not enough new blocks, sleeping 60 seconds"
                     threadDelay $ 60 * 1000000

              _ -> do
                logDebug "Backnotarisation not found, proceed to backnotarise"
                opret <- getBackNotarisation nc ethnota
                notariseToKMD nc opret

  where
    getNotariserConfig configName = do
      gateway <- asks getEthGateway
      (threshold, members) <- ethCallABI gateway "getMembers()" ()
      JsonInABI nc <- ethCallABI gateway "getConfig(string)" (configName :: Text)
      pure $ nc { members, threshold }

    checkConfig NotariserConfig{..} (EthIdent _ addr) = do
      when (majorityThreshold (length members) < kmdNotarySigs) $ do
        logError "Majority threshold is less than required notary sigs"
        impureThrow ConfigException 
      when (not $ elem addr members) $ do
        logError "I am not in the members list"
        impureThrow ConfigException

    runForever act = forever $ act `catches` handlers
      where
        handlers =
          [ Handler $ \e -> recover logInfo 5 (e :: ConsensusException)
          , Handler $ \e -> recover logWarn 60 (fmtHttpException e)
          , Handler $ \e -> recover logWarn 60 (e :: RPCException)
          , Handler $ \e -> recover logError 600 (e :: ConfigException)
          ]
        recover f d e = do
          f $ show e
          liftIO $ threadDelay $ d * 1000000
        fmtHttpException (HttpExceptionRequest _ e) = e


-- TODO: need error handling here with strategies for configuration errors, member mischief etc.
notariseToETH :: NotariserConfig -> Word32 -> Zeno EthNotariser ()
notariseToETH NotariserConfig{..} height32 = do

  let height = fromIntegral height32
  logDebug $ "Notarising from block %i" % height

  ident@(EthIdent _ myAddress) <- asks has
  gateway <- asks getEthGateway
  let cparams = ConsensusParams members ident consensusTimeout
  r <- ask
  let run = liftIO . runZeno r

  -- we already have all the data for the call to set the new block height
  -- in our ethereum contract. so create the call.

  blockHash <- bytes . unHex <$> queryBitcoin "getblockhash" [height]
  let notariseCallData = abi "notarise(uint256,bytes32,bytes)"
                             (height, blockHash :: Bytes 32, "" :: ByteString)
      proxyParams = (notarisationsContract, height, notariseCallData)
      sighash = ethMakeProxySigMessage proxyParams

  -- Ok now we have all the parameters together, we need to collect sigs and get the tx

  runConsensus cparams proxyParams $ do
    {- The trick is, that during this whole block inside runConsensus,
       each step will stay open until the end so that lagging nodes can
       join in late. -}

    run $ logDebug "Step 1: Collect sigs"
    sigBallots <- stepWithTopic sighash (collectThreshold threshold) ()

    run $ logDebug "Step 2: Get proposed transaction"
    let proxyCallData = ethMakeProxyCallData proxyParams (bSig <$> unInventory sigBallots)
    ballot@(Ballot proposer _ tx) <- propose $ run $ ethMakeTransaction gateway proxyCallData
    run $ checkTxProposed ballot

    -- There's a bit of an open question here: A single node is selected to create the transaction,
    -- and many nodes can indeed submit it but they may encounter errors depending on how fast their
    -- ethereum nodes sync, because it'll appear as a double spend to Ethereum. So the question is what
    -- to do, do we trust the proposer to submit the transaction (they could just do so anyway), or
    -- do all nodes submit the transactions 

    let txid = hashTx tx

    run $ logDebug "Step 3: Confirm proposal"
    _ <- step collectMajority ()

    run $
      if (proposer == myAddress)
        then do
          logDebug "Step 4: Submit transaction"
          r <- postTransaction tx
          liftIO $ do
            print tx
            print txid
            print r
        else do
          logDebug $ "Step 4: Proposer will submit: " ++ show txid

    run $ logDebug "Step 5: Confirmed that tx was sumbmitted by proposer"
    -- This will timeout if proposer had an exception while submitting the transaction
    _ <- step (collectMembers [proposer]) ()
  
    run do
      logDebug $ "Step 6: Wait for transaction confirmation on chain"
      waitTransactionConfirmed1 (120 * 1000000) txid
      pure ()


getLastNotarisationOnEth :: NotariserConfig -> Zeno EthNotariser (Maybe NotarisationOnEth)
getLastNotarisationOnEth NotariserConfig{..} = do
  r <- ethCallABI notarisationsContract "getLastNotarisation()" ()
  pure $
    case r of
      NOE 0 _ _ _ -> Nothing
      noe -> Just noe


getBackNotarisation :: NotariserConfig -> NotarisationOnEth -> Zeno EthNotariser NotarisationData
getBackNotarisation NotariserConfig{..} NOE{..} = do
  pure $ NOR (sha3AsBytes32 foreignHash) foreignHeight kmdChainSymbol (sha3AsBytes32 nullSha3) 0 0


checkTxProposed :: Ballot Transaction -> Zeno EthNotariser ()
checkTxProposed (Ballot sender _ tx) = do
  case recoverFrom tx of
    Nothing -> throw $ ConsensusMischief $ "Can't recover sender from tx"
    Just s | s /= sender -> throw $ ConsensusMischief $ "Sender wrong"
    _ -> pure ()
