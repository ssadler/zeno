
module Zeno.Notariser.KMDETH where

import Data.List ((\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
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
import Zeno.Notariser.Collect
import Zeno.Notariser.KMDDpow
import Zeno.Notariser.Shuffle
import Zeno.Notariser.Targets
import Zeno.Notariser.Types
import Zeno.Notariser.Synchronous
import Zeno.Consensus
import Zeno.Console
import Zeno.Prelude


runNotariseKmdToEth :: PubKey -> Address -> ConsensusNetworkConfig -> GethConfig -> FilePath -> ConsoleArgs -> IO ()
runNotariseKmdToEth pk gateway networkConfig gethConfig kmdConfPath consoleArgs = do
  runZeno defaultLog () do
    withConsole consoleArgs LevelDebug do
      threadDelay 1000000
      bitcoinConf <- loadBitcoinConfig kmdConfPath
      let kmdAddress = deriveKomodoAddress pk
      wif <- withContext (const bitcoinConf) $ queryBitcoin "dumpprivkey" [kmdAddress]
      sk <- either error pure $ parseWif komodo wif
      let ethIdent = deriveEthIdent sk
      let kmdIdent = deriveKomodoIdent sk

      withConsensusNode networkConfig do
        let toNotariser node = EthNotariser bitcoinConf node gethConfig gateway sk ethIdent kmdIdent
        withContext toNotariser do
          KomodoIdent{..} <- asks has
          EthIdent{..} <- asks has
          logInfo $ "KMD address: " ++ show kmdAddress
          logInfo $ "ETH address: " ++ show ethAddress
          runKmdThreads
          runNotariserForever


runNotariserForever :: Zeno EthNotariser ()
runNotariserForever = do
  runForever do                               -- Run and handle variety of exceptions
    withLocalResources do                     -- All threads killed if there's an exception
      nc <- getNotariserConfig "KMDETH"       -- Config will be refeshed if there is an exception
      asks has >>= validateConfig nc
      runRepeatedly do
        runNotariserSync nc $ notariserSyncFree nc

  where
    getNotariserConfig configName = do
      gateway <- asks getEthGateway
      (threshold, members) <- gatewayGetMembers gateway
      JsonInABI nc <- ethCallABI gateway "getConfig(string)" (configName :: Text)
      pure $ nc { members, threshold }

    validateConfig NotariserConfig{..} (EthIdent _ addr)
      | not (elem addr members)                      = ce "I am not in the members list"
      | length members < (kmdNotarySigs sourceChain) = ce "Not enough members to sign tx on KMD"
      | length members < threshold                   = ce "Not enough members to sign tx on ETH"
      | otherwise = pure ()
      where ce = throwIO . ConfigException

    runForever act = forever $ act `catches` handlers
      where
        handlers =
          [ Handler $ \e -> recover logError 10 (e :: RPCException)
          , Handler $ \e -> recover logError 20 (e :: RPCTransportException)
          , Handler $ \e -> recover logError 60 (e :: ConfigException)
          ]
        recover f d e = do
          f $ show e
          liftIO $ threadDelayS d

    runRepeatedly act = do
      let maxCount = 10
      fix1 0 \f i -> do
        when (i < maxCount) do
          join do
            catches
              do act >> pure (f $ i+1)
              [ Handler $ \ConsensusTimeout -> pure $ f $ i+3
              , Handler $ \e@(ConsensusInvalidProposal s) -> do
                  logWarn $ show e
                  pure $ f $ i+1
              ]


runNotariserSync :: NotariserConfig
                 -> NotariserSync KMDSource ETHDest (Zeno EthNotariser) ()
                 -> Zeno EthNotariser ()
runNotariserSync nc@NotariserConfig{sourceChain=s@KMDSource{..}, destChain=d@ETHDest{..}, ..} skel = do
  sendUI $ UI_Process $ Just $ UIOther "Inspect chains"
  go skel
  where
  go = deboneBy \case

    Return () -> pure ()

    NotariserSyncLift act :>>= f -> act >>= go . f

    WaitNextSourceHeight height :>>= f -> waitNextNotariseHeight s height >>= go . f
    WaitNextDestHeight height :>>= f -> waitNextNotariseHeight d height >>= go . f

    GetLastNotarisation :>>= f -> do
      ethGetLastNotarisation ethNotarisationsContract >>= go . f

    GetLastNotarisationReceipt :>>= f -> do
      kmdGetLastNotarisationData kmdSymbol >>= go . f

    RunNotarise sourceHeight mlastReceipt :>>= f -> do
      blockHash <- queryBitcoin "getblockhash" [sourceHeight]
      let params = (sourceHeight, blockHash, "")
          label = "%s.%i ⇒  %s" % (getSymbol s, sourceHeight, getSymbol d)
      notariseToETH nc label params >>= go . f

    RunNotariseReceipt destHeight NOE{..} :>>= f -> do
      let
        label = "%s.%i ⇒  %s" % (getSymbol d, destHeight, getSymbol s)
        receipt = NOR
          { norBlockHash = noeForeignHash
          , norBlockNumber = noeForeignHeight
          , norForeignRef = (destHeight, nullRAddress, minBound)
          , norSymbol = kmdSymbol
          , norMom = minBound
          , norMomDepth = 0
          , norCcId = 0
          }
      notariseKmdDpow nc label receipt >>= go . f


notariseToETH :: NotariserConfig -> String -> EthNotarisationParams -> Zeno EthNotariser ()
notariseToETH nc@NotariserConfig{..} label notarisationParams = do
  r <- ask
  let
    ETHDest{..} = destChain
    EthIdent{..} = has r
    height = fromIntegral $ view _1 notarisationParams
    proxyParams = (ethNotarisationsContract, height, ethMakeNotarisationCallData notarisationParams)
    proxySigHash = ethMakeProxySigMessage proxyParams

  cparams <- getConsensusParams nc KmdToEth

  r <- runConsensus label cparams proxyParams do
    -- First complete an empty step to make sure everyone is on the same page
    _ <- step "init" () collectMajority
    dist@(proposer:_) <- roundShuffle members
    proposal <-
      step "sigs" proxySigHash $
        \recv -> do
          if ethAddress == proposer
             then do
               chosen <- collectWeighted dist threshold recv
               let proxyCallData = ethMakeProxyCallData proxyParams (bSig <$> unInventory chosen)
               tx <- lift $ ethMakeNotarisationTx nc proxyCallData
               pure $ Just (chosen, tx) -- Send chosen sigs to make it easy to reconstruct tx
           else do
             collectWith (\_ _ -> pure Nothing) recv
    stepOptData "tx" proposal $ collectMember proposer


  --- Validate the transaction and dispatch to chain

  let Ballot proposer _ (chosenSigs, tx) = r
  sigs <- either invalidProposal pure $ validateSigs nc proxySigHash chosenSigs
  validateProposedTx nc proxyParams sigs proposer tx
  handle onTransactionPostError $ void $ eth_sendRawTransaction tx
  let txid = hashTx tx
  sendUI $ UI_Step "Confirm TX"
  waitTransactionConfirmed1 (120 * 1000000) txid >>=
    \case
      Nothing -> do
        -- This is the fast path fail; we submitted the tx but our node doesn't know
        -- about it, so it must be invalid.
        invalidProposal $ show tx
      Just height -> do
        logInfo $ "Tx confirmed in block %s: %s" % (show height, show txid)


onTransactionPostError :: RPCException -> Zeno r ()
onTransactionPostError (RPCError (String s))
  | T.isInfixOf "known transaction" (T.toLower s) = pure ()
  | T.isInfixOf "already known" (T.toLower s) = pure ()
onTransactionPostError (RPCError o)
  | otherwise = logTrace debugTraceRPC $ "Got submission error, maybe acceptable: " ++ toS o
onTransactionPostError e = throwIO e


invalidProposal :: String -> Zeno EthNotariser a
invalidProposal = throwIO . ConsensusInvalidProposal


ethMakeNotarisationTx :: NotariserConfig -> ByteString -> Zeno EthNotariser Transaction
ethMakeNotarisationTx nc@NotariserConfig{..} callData = do
  let ETHDest{..} = destChain
  EthIdent sk myAddress <- asks has
  U256 gasPriceRec <- queryEthereum "eth_gasPrice" ()
  let gasPrice = gasPriceRec + quot gasPriceRec 2         -- Fixed gas price increase
  nonce <- eth_getTransactionCount myAddress
  tx <- ethMakeNotarisationTxPartial nc callData <&>
    \tx -> tx { _nonce = nonce, _gasPrice = gasPrice }
  pure $ signTx sk tx

ethMakeNotarisationTxPartial :: NotariserConfig -> ByteString -> Zeno EthNotariser Transaction
ethMakeNotarisationTxPartial NotariserConfig{..} callData = do
  let ETHDest{..} = destChain
  gateway <- asks getEthGateway
  pure $ Tx (error "nonce") 0 (Just gateway) Nothing (error "gasPrice") ethNotariseGas callData ethChainId

validateSigs :: NotariserConfig -> Bytes32 -> Inventory Bytes32 -> Either String [RecSig]
validateSigs NotariserConfig{..} ourHash chosenInv = do
  when (length chosenInv /= threshold) do
    Left $ "Wrong number of sigs, expected %i, got %i" % (threshold, length chosenInv)

  forM (Map.toList chosenInv)
    \(addr, (sig, theirhash)) -> do
      when (not $ elem addr members) do
        Left "Contains non members"
      when (theirhash /= ourHash) do
        Left "Hash is not the same"
      let mrecoveredAddr = recoverAddr ourHash sig
      when (mrecoveredAddr /= Right addr) do
        Left "Invalid signature provided"
      pure sig

validateProposedTx
  :: NotariserConfig -> ProxyParams -> [RecSig]
  -> Address -> Transaction -> Zeno EthNotariser ()
validateProposedTx nc@NotariserConfig{..} proxyParams sigs sender tx = do

  recoverFrom tx &
    \case
      Left _ -> invalidProposal "Can't recover sender from tx"
      Right s | s /= sender -> invalidProposal "Sender wrong"
      _ -> pure ()

  minGasPrice <- eth_gasPrice
  when (_gasPrice tx < minGasPrice) do
    invalidProposal "Gas price too low"

  let callData = ethMakeProxyCallData proxyParams sigs
  reconstructed <-
    ethMakeNotarisationTxPartial nc callData <&>
      \txre -> txre { _gasPrice = _gasPrice tx, _nonce = _nonce tx, _sig = _sig tx }

  when (reconstructed /= tx) do
    logDebug $ show tx
    logDebug $ show reconstructed
    invalidProposal "could not reconstruct tx"
