
module Zeno.Notariser.KMDETH where

import qualified Data.Map.Strict as Map
import Data.List ((\\))
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
          runNotariserForever


runNotariserForever :: Zeno EthNotariser ()
runNotariserForever = do
  runForever do                               -- Run and handle variety of exceptions
    withLocalResources do                     -- All threads killed if there's an exception
      nc <- getNotariserConfig "KMDETH"       -- Config will be refeshed if there is an exception
      asks has >>= validateConfig nc
      runRepeatedly do
        runNotariserStep nc $ notariserStepFree nc

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
            catch
              do act            >> pure (f (i + 1))           -- Config reload every n notarisations
              \ConsensusTimeout -> pure (f (i + 3))           -- Faster if there are timeouts


runNotariserStep :: NotariserConfig
                 -> NotariserStep KMDSource ETHDest (Zeno EthNotariser) a
                 -> Zeno EthNotariser a
runNotariserStep nc@NotariserConfig{sourceChain=s@KMDSource{..}, destChain=d@ETHDest{..}, ..} = go
  where
  go = deboneBy \case

    NotariserStepLift act :>>= f -> act >>= go . f

    WaitNextSourceHeight height :>>= f -> waitNextNotariseHeight s height >>= go . f
    WaitNextDestHeight height :>>= f -> waitNextNotariseHeight d height >>= go . f

    GetLastNotarisationReceiptFree :>>= f -> do
      kmdGetLastNotarisationData kmdSymbol >>= go . f

    GetLastNotarisationFree :>>= f -> do
      ethGetLastNotarisationAndSequence ethNotarisationsContract >>= go . f

    RunNotarise seq sourceHeight mlastReceipt :>>= f -> do
      blockHash <- queryBitcoin "getblockhash" [sourceHeight]
      let params = (sourceHeight, blockHash, "")
          label = "%s.%i ⇒  %s" % (getSymbol s, sourceHeight, getSymbol d)
      notariseToETH nc label seq params >>= go . f

    RunNotariseReceipt seq destHeight NOE{..} :>>= f -> do
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
      notariseKmdDpow nc label seq receipt >>= go . f


notariseToETH :: NotariserConfig -> String -> ProposerSequence -> NotarisationParams -> Zeno EthNotariser ()
notariseToETH nc@NotariserConfig{..} label seq notarisationParams = do
  let ETHDest{..} = destChain
  let height = fromIntegral $ view _1 notarisationParams

  ident@(EthIdent _ myAddress) <- asks has

  r <- ask
  let
    run :: Zeno EthNotariser a -> Consensus a
    run = withContext (const r)

    notariseCallData = ethMakeNotarisationCallData notarisationParams
    proxyParams = (ethNotarisationsContract, height, notariseCallData)
    proxySigHash = ethMakeProxySigMessage proxyParams

  cparams <- getConsensusParams nc KmdToEth
  runConsensus label cparams proxyParams do

    sigsInv <- step "tx sigs" (collectThreshold threshold) proxySigHash

    let chosen = Map.take threshold sigsInv
        proxyCallData = ethMakeProxyCallData proxyParams (bSig <$> unInventory chosen)
        proposal = do
          tx <- run $ ethMakeNotarisationTx nc proxyCallData
          pure (chosen, tx) -- Send chosen sigs to make it easy to reconstruct tx

    txid <-
      proposeWithAction "tx sender" (Just seq) proposal
        \(Ballot pAddr sig (chosenSigs, tx)) -> do
          run do
            sigs <- either invalidProposal pure $ validateSigs nc proxySigHash chosenSigs
            validateProposedTx nc proxyParams sigs pAddr tx
            catch
              do postTransaction tx
              \e -> do
                invalidProposal $
                  "Exception posting proposed transaction: %s\n%s" %
                    (show tx, show (e :: RPCException))

    incStep "wait for tx confirm ..."
    run $ waitTransactionConfirmed1 (120 * 1000000) txid
    logInfo "Transaction confirmed"
    pure ()


invalidProposal :: String -> Zeno EthNotariser a
invalidProposal = throwIO . ConsensusInvalidProposal


ethMakeNotarisationTx :: NotariserConfig -> ByteString -> Zeno EthNotariser Transaction
ethMakeNotarisationTx nc@NotariserConfig{..} callData = do
  let ETHDest{..} = destChain
  EthIdent sk myAddress <- asks has
  U256 gasPriceRec <- queryEthereum "eth_gasPrice" ()
  let gasPrice = gasPriceRec + quot gasPriceRec 2         -- Fixed gas price increase
  tx <- ethMakeNotarisationTxWithGas nc myAddress gasPrice callData
  signTx sk tx

ethMakeNotarisationTxWithGas
  :: NotariserConfig -> Address -> Integer -> ByteString -> Zeno EthNotariser Transaction
ethMakeNotarisationTxWithGas NotariserConfig{..} address gasPrice callData = do
  let ETHDest{..} = destChain
  gateway <- asks getEthGateway
  nonce <- queryAccountNonce address
  pure $ Tx nonce 0 (Just gateway) Nothing gasPrice ethNotariseGas callData ethChainId

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
      let recoveredAddr = unsafePerformIO $ recoverAddr ourHash sig
      when (recoveredAddr /= addr) do
        Left "Invalid signature provided"
      pure sig

validateProposedTx
  :: NotariserConfig -> ProxyParams -> [RecSig] -> Address -> Transaction -> Zeno EthNotariser ()
validateProposedTx nc@NotariserConfig{..} proxyParams sigs sender tx = do

  recoverFrom tx >>=
    \case
      Nothing -> invalidProposal "Can't recover sender from tx"
      Just s | s /= sender -> invalidProposal "Sender wrong"
      _ -> pure ()

  minGasPrice <- eth_gasPrice
  when (_gasPrice tx < minGasPrice) do
    invalidProposal "Gas price too low"

  let callData = ethMakeProxyCallData proxyParams sigs
  reconstructed <-
    ethMakeNotarisationTxWithGas nc sender (_gasPrice tx) callData
      <&> (\tx -> tx { _sig = _sig tx })

  when (reconstructed /= tx) do
    invalidProposal "could not reconstruct tx"
