
module Zeno.Notariser.KMDETH where

import Data.Serialize

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
import Zeno.Notariser.Targets
import Zeno.Notariser.Types
import Zeno.Notariser.Stats
import Zeno.Notariser.Step
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
      kmdAddress <- deriveKomodoAddress pk
      wif <- withContext (const bitcoinConf) $ queryBitcoin "dumpprivkey" [kmdAddress]
      sk <- either error pure $ parseWif komodo wif
      ethIdent <- deriveEthIdent sk
      kmdIdent <- deriveKomodoIdent sk

      withConsensusNode networkConfig do
        let toNotariser node = EthNotariser bitcoinConf node gethConfig gateway sk ethIdent kmdIdent
        withContext toNotariser do
          KomodoIdent{..} <- asks has
          EthIdent{..} <- asks has
          logInfo $ "KMD address: " ++ show kmdAddress
          logInfo $ "ETH address: " ++ show ethAddress
          runKmdThreads
          runNotariser


runNotariser :: Zeno EthNotariser ()
runNotariser = do
  runForever do                               -- Run and handle variety of exceptions
    withLocalResources do                     -- All threads killed if there's an exception
      nc <- getNotariserConfig "KMDETH"       -- Config will be refeshed if there is an exception
      asks has >>= checkConfig nc
      runRepeatedly do
        runNotariserStep nc $ notariserStepFree nc

  where
    getNotariserConfig configName = do
      gateway <- asks getEthGateway
      (threshold, members) <- gatewayGetMembers gateway
      JsonInABI nc <- ethCallABI gateway "getConfig(string)" (configName :: Text)
      pure $ nc { members, threshold }

    checkConfig NotariserConfig{..} (EthIdent _ addr)
      | not (elem addr members)                      = ce "I am not in the members list"
      | length members < (kmdNotarySigs sourceChain) = ce "Not enough members to sign tx on KMD"
      | length members < threshold                   = ce "Not enough members to sign tx on ETH"
      | otherwise = pure ()
      where ce = throwIO . ConfigException

    runForever act = forever $ act `catches` handlers
      where
        handlers =
          [ Handler $ \e -> recover logWarn  20 (fmtHttpException e)
          , Handler $ \e -> recover logWarn  20 (e :: RPCException)
          , Handler $ \e -> recover logError 60 (e :: ConfigException)
          ]
        recover f d e = do
          f $ show e
          liftIO $ threadDelayS d
        fmtHttpException (HttpExceptionRequest _ e) = e
        fmtHttpException e = error ("Configuration error: " ++ show e)

    runRepeatedly act = do
      let maxCount = 10
      fix1 0 \f i -> do
        when (i < maxCount) do
          join do
            catch
              do act            >> pure (f (i + 1))           -- Config reload every n notarisations
              \ConsensusTimeout -> pure (f (i + 3))           -- Faster if there are timeouts


runNotariserStep :: NotariserConfig
                 -> NotariserStep KMDSource ETHDest (Zeno EthNotariser) a
                 -> Zeno EthNotariser a
runNotariserStep nc@NotariserConfig{sourceChain=s@KMDSource{..}, destChain=d@ETHDest{..}, ..} =
  iterT \case

    WaitNextSourceHeight height f -> waitNextNotariseHeight s height >>= f
    WaitNextDestHeight height f -> waitNextNotariseHeight d height >>= f

    GetLastNotarisationReceiptFree f -> do
      kmdGetLastNotarisationData kmdSymbol >>= f

    GetLastNotarisationFree f -> do
      ethGetLastNotarisationAndSequence ethNotarisationsContract >>= f

    RunNotarise seq sourceHeight mlastReceipt f -> do
      blockHash <- queryBitcoin "getblockhash" [sourceHeight]
      let params = (sourceHeight, blockHash, "")
          label = "%s.%i ⇒  %s" % (getSymbol s, sourceHeight, getSymbol d)
      notariseToETH nc label seq params >>= f

    RunNotariseReceipt seq destHeight NOE{..} f -> do
      let
        label = "%s.%i ⇒  %s" % (getSymbol d, destHeight, getSymbol s)
        receipt = NOR
          { norBlockHash = noeForeignHash
          , norBlockNumber = noeForeignHeight
          , norForeignRef = (destHeight, minBound)
          , norSymbol = kmdSymbol
          , norMom = minBound
          , norMomDepth = 0
          , norCcId = 0
          }
      notariseKmdDpow nc label seq receipt >>= f


notariseToETH :: NotariserConfig -> String -> ProposerSequence -> NotarisationParams -> Zeno EthNotariser ()
notariseToETH nc@NotariserConfig{..} label seq notarisationParams = do
  let ETHDest{..} = destChain
  let height = fromIntegral $ view _1 notarisationParams

  ident@(EthIdent _ myAddress) <- asks has
  cparams <- getConsensusParamsWithStats nc KmdToEth
  r <- ask
  let
    run :: Zeno EthNotariser a -> Consensus a
    run = withContext (const r)

    notariseCallData = ethMakeNotarisationCallData notarisationParams
    proxyParams = (ethNotarisationsContract, height, notariseCallData)

  runConsensus label cparams proxyParams do

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
    -- do all nodes submit the transactions? Probably a subset should submit.

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
  let ETHDest{..} = destChain
  EthIdent sk myAddress <- asks has
  gateway <- asks getEthGateway
  nonce <- queryAccountNonce myAddress
  U256 gasPriceRec <- queryEthereum "eth_gasPrice" ()
  let gasPrice = gasPriceRec + quot gasPriceRec 2         -- Fixed gas price increase
      tx = Tx nonce 0 (Just gateway) Nothing gasPrice ethNotariseGas callData ethChainId
  signTx sk tx


checkTxProposed :: Ballot Transaction -> Zeno EthNotariser ()
checkTxProposed (Ballot sender _ tx) = do
  recoverFrom tx >>=
    \case
      Nothing -> throwIO $ ConsensusMischief sender "Can't recover sender from tx"
      Just s | s /= sender -> throwIO $ ConsensusMischief sender "Sender wrong"
      _ -> pure ()
