
module Zeno.Notariser.KMDETH where

import Network.Bitcoin
import Network.Ethereum
import Network.Ethereum.Transaction
import Network.Komodo
import Network.HTTP.Simple
import Network.JsonRpc

import Zeno.EthGateway
import Zeno.Notariser.Common
import Zeno.Notariser.Common.KMD
import Zeno.Notariser.KMDDpow
import Zeno.Notariser.Types
import Zeno.Notariser.Stats
import Zeno.Consensus
import Zeno.Console
import Zeno.Prelude


runNotariseKmdToEth :: PubKey -> Address -> ConsensusNetworkConfig -> GethConfig -> FilePath -> Bool -> IO ()
runNotariseKmdToEth pk gateway networkConfig gethConfig kmdConfPath useui = do
  runZeno defaultLog () do
    let withUI = if useui then withConsoleUI LevelInfo else id
    withUI do
      threadDelay 1000000
      bitcoinConf <- loadBitcoinConfig kmdConfPath
      let kmdAddress = deriveKomodoAddress pk
      wif <- withContext (const bitcoinConf) $ queryBitcoin "dumpprivkey" [kmdAddress]
      sk <- either error pure $ parseWif komodo wif

      withConsensusNode networkConfig do
        let toNotariser node = EthNotariser bitcoinConf node gethConfig gateway sk
        withContext toNotariser do
          KomodoIdent{..} <- asks has
          EthIdent{..} <- asks has
          logInfo $ "KMD address: " ++ show kmdAddress
          logInfo $ "ETH address: " ++ show ethAddress

          runKmdThreads

          runForever do
            nc <- getNotariserConfig "KMDETH"
            asks has >>= checkConfig nc
            -- Config will be refeshed if there is an exception
            forever $ notariserStep nc

  where
    getNotariserConfig configName = do
      gateway <- asks getEthGateway
      (threshold, members) <- gatewayGetMembers gateway
      JsonInABI nc <- ethCallABI gateway "getConfig(string)" (configName :: Text)
      pure $ nc { members, threshold }

    checkConfig NotariserConfig{..} (EthIdent _ addr)
      | not (elem addr members)        = ce "I am not in the members list"
      | length members < kmdNotarySigs = ce "Not enough members to sign tx on KMD"
      | length members < threshold     = ce "Not enough members to sign tx on ETH"
      | otherwise = pure ()
      where ce = throwIO . ConfigException

    runForever act = forever $ act `catches` handlers
      where
        handlers =
          [ Handler $ \e -> recover logWarn 20 (fmtHttpException e)
          , Handler $ \e -> recover logWarn 20 (e :: RPCException)
          , Handler $ \e -> recover logError 60 (e :: ConfigException)
          ]
        recover f d e = do
          f $ show e
          liftIO $ threadDelay $ d * 1000000
        fmtHttpException (HttpExceptionRequest _ e) = e
        fmtHttpException e = error ("Configuration error: " ++ show e)


notariserStep :: NotariserConfig -> Zeno EthNotariser ()
notariserStep nc@NotariserConfig{..} = do
  getLastNotarisationOnEth nc >>= handleTimeout . go
  where
  handleTimeout = handle \(ConsensusTimeout _) -> mempty

  go Nothing = do
    logInfo "No prior notarisations found"
    forward 0 0

  go (Just (ethnota@NOE{..}, sequence)) = do
    logDebug $ "Found notarisation on ETH for %s height %i" % (kmdChainSymbol, foreignHeight)

    getLastNotarisation kmdChainSymbol >>=
      \case
        Just (Notarisation _ _ (BND NOR{..}))
          | blockNumber == foreignHeight -> do
            logDebug "Found backnotarisation, proceed with next notarisation"
            forward sequence foreignHeight
          | blockNumber > foreignHeight -> do
            logError $ show NOR{..}
            logError $ show ethnota
            logError "The backnotarised height in KMD is higher than the notarised\
                     \ height in ETH. Is ETH node is lagging? Proceeding anyway."
            forward sequence foreignHeight

        _ -> do
          logDebug "Backnotarisation not found, proceed to backnotarise"
          opret <- getBackNotarisation nc ethnota
          let seq = shiftSequence 2 sequence
          notariseKmdDpow nc seq opret

  forward sequence lastNotarisedHeight = do
      newHeight <- waitKmdNotariseHeight kmdBlockInterval lastNotarisedHeight
      notariseToETH nc sequence newHeight

  shiftSequence n seq =
    seq + ProposerSequence (quot (length members) n)


notariseToETH :: NotariserConfig -> ProposerSequence -> Word32 -> Zeno EthNotariser ()
notariseToETH nc@NotariserConfig{..} seq height32 = do

  let height = fromIntegral height32
  logDebug $ "Notarising from block %i" % height

  ident@(EthIdent _ myAddress) <- asks has
  gateway <- asks getEthGateway
  cparams <- getConsensusParamsWithStats nc KmdToEth
  r <- ask
  let run = withContext (const r)
      roundLabel = "kmd.%i ⇒  eth" % height

  -- we already have all the data for the call to set the new block height
  -- in our ethereum contract. so create the call.

  blockHash <- queryBitcoin "getblockhash" [height]
  let notariseCallData = abi "notarise(uint256,bytes32,bytes)"
                             (height, blockHash :: Bytes32, "" :: ByteString)
      proxyParams = (notarisationsContract, height, notariseCallData)

  -- Ok now we have all the parameters together, we need to collect sigs and get the tx

  runConsensus roundLabel cparams proxyParams do

    sigBallots <- step "tx sigs" (collectThreshold threshold)
                                 (ethMakeProxySigMessage proxyParams)

    let proxyCallData = ethMakeProxyCallData proxyParams (bSig <$> unInventory sigBallots)
        buildTx = run $ ethMakeNotarisationTx nc proxyCallData
    ballot@(Ballot proposer _ tx) <- propose "tx sender" (Just seq) buildTx
    run $ checkTxProposed ballot

    -- There's a bit of an open question here: A single node is selected to create the transaction,
    -- and many nodes can indeed submit it but they may encounter errors depending on how fast their
    -- ethereum nodes sync, because it'll appear as a double spend to Ethereum. So the question is what
    -- to do, do we trust the proposer to submit the transaction (they could just do so anyway), or
    -- do all nodes submit the transactions 

    let txid = hashTx tx

    _ <- step "confirm tx" collectMajority ()

    run $
      if proposer == myAddress
         then postTransaction tx >> pure ()
         else logInfo $ "Proposer will submit: " ++ show txid

    -- This will timeout if proposer had an exception while submitting the transaction
    _ <- step "confirm submit" (collectMembers [proposer]) ()

    -- Check that everyone else confirmed the proposer
    _ <- step "confirm submit" collectMajority ()
  
    incStep "wait for tx confirm ..."
    run $ waitTransactionConfirmed1 (120 * 1000000) txid
    logInfo "Transaction confirmed"
    pure ()


ethMakeNotarisationTx :: NotariserConfig -> ByteString -> Zeno EthNotariser Transaction
ethMakeNotarisationTx NotariserConfig{..} callData = do
  EthIdent sk myAddress <- asks has
  gateway <- asks getEthGateway
  nonce <- queryAccountNonce myAddress
  U256 gasPriceRec <- queryEthereum "eth_gasPrice" ()
  let gasPrice = gasPriceRec + quot gasPriceRec 2
      tx = Tx nonce 0 (Just gateway) Nothing gasPrice ethNotariseGas callData ethChainId
  pure $ signTx sk tx


getLastNotarisationOnEth :: NotariserConfig
                         -> Zeno EthNotariser (Maybe (NotarisationOnEth, ProposerSequence))
getLastNotarisationOnEth NotariserConfig{..} = do
  (r, sequence) <- ethCallABI notarisationsContract "getLastNotarisation()" ()
  pure $
    case r of
      NOE 0 _ _ _ -> Nothing
      _ -> Just (r, ProposerSequence sequence)


getBackNotarisation :: NotariserConfig -> NotarisationOnEth -> Zeno EthNotariser NotarisationData
getBackNotarisation NotariserConfig{..} NOE{..} = do
  pure $ NOR
    { blockHash = (sha3AsBytes32 foreignHash)
    , blockNumber = foreignHeight
    , txHash = newFixed 0xFF
    , symbol = kmdChainSymbol
    , mom = nullBytes
    , momDepth = 0
    , ccId = 0
    , momom = nullBytes
    , momomDepth = 0
    }


checkTxProposed :: Ballot Transaction -> Zeno EthNotariser ()
checkTxProposed (Ballot sender _ tx) = do
  case recoverFrom tx of
    Nothing -> throwIO $ ConsensusMischief sender "Can't recover sender from tx"
    Just s | s /= sender -> throwIO $ ConsensusMischief sender "Sender wrong"
    _ -> pure ()
