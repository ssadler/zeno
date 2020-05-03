
module Zeno.Notariser.KMDETH where

import Data.Aeson.TH

import Network.Bitcoin
import Network.Ethereum
import Network.Komodo
import Network.Ethereum.Transaction
import qualified Network.Haskoin.Prelude as H

--import Zeno.Notariser.UTXO

import Zeno.EthGateway
import Zeno.Consensus
import Zeno.Config
import Zeno.Prelude
import Zeno.Prelude.Lifted


kmdInputAmount :: Word64
kmdInputAmount = 9800

consensusTimeout :: Int
consensusTimeout = 5 * 1000000


data EthNotariser = EthNotariser
  { getKomodoConfig :: BitcoinConfig
  , getNode :: ConsensusNode
  , gethConfig :: GethConfig
  , getEthGateway :: EthGateway
  , getSecret :: SecKey
  }

newtype NotariserConfig = NotariserConfig
  { notarisationsContract :: Address
  } deriving (Show)

$(deriveJSON defaultOptions ''NotariserConfig)

instance Has GethConfig    EthNotariser where has = gethConfig
instance Has BitcoinConfig EthNotariser where has = getKomodoConfig
instance Has ConsensusNode EthNotariser where has = getNode
instance Has EthGateway    EthNotariser where has = getEthGateway
instance Has EthIdent      EthNotariser where has = toEthIdent . getSecret



-- :main notarise kmdeth --host 127.0.0.1 --port=40440 --seed=127.0.0.1:40440 --address=RWgagrqdN7YWH4N6kB4mWCNPCgtAMkCLFp --geth http://127.0.0.1:9545/

runNotariseKmdToEth :: GethConfig -> ConsensusNetworkConfig -> EthGateway -> FilePath -> RAddress -> IO ()
runNotariseKmdToEth gethConfig consensusConfig gateway kmdConfPath kmdAddr = do
  --threadDelay 2000000
  bitcoinConf <- loadBitcoinConfig kmdConfPath
  wif <- runZeno bitcoinConf $ queryBitcoin "dumpprivkey" [kmdAddr]

  ident <-
    case H.fromWif komodo wif of
          Just (H.SecKeyI seckey True) -> pure seckey
          _ -> error $ "Couldn't parse WIF from daemon: " <> show wif

  withConsensusNode consensusConfig $
    \node -> do
      let config = EthNotariser bitcoinConf node gethConfig gateway ident
      runZeno config ethNotariser


ethNotariser :: Zeno EthNotariser ()
ethNotariser = do
  (_, _, kmdAddr) <- deriveKomodoIdent <$> asks getSecret
  logInfo $ "My KMD address: " ++ show kmdAddr
  (EthIdent _ ethAddr) <- asks has
  logInfo $ "My ETH address: " ++ show ethAddr

  -- forkMonitorUTXOs kmdInputAmount 5 50 ident

  forever $ do
    -- get last notarisation height from ETH
    r <- gatewayGetConfigJson "KMDETH"

    blockRange@(start, end) <- getBlockRange r
    logInfo $ printf "KMD Block range: %i to %i" start end
    if start >= end
       then do
         logInfo "Waiting for more blocks"
         threadDelay $ 180 * 1000000
        else do
          runNotariserConsensus r blockRange


-- TODO: need error handling here with strategies for configuration errors, member mischief etc.
runNotariserConsensus :: NotariserConfig -> (Integer, Integer) -> Zeno EthNotariser ()
runNotariserConsensus NotariserConfig{..} (_, height) = do
  ident <- asks has
  (EthIdent _ ethAddr) <- asks has
  gateway <- asks getEthGateway
  (threshold, members) <- gatewayGetMembers
  let cparams = ConsensusParams members ident consensusTimeout
  r <- ask
  let run = liftIO . runZeno r


  -- we already have all the data for the call to set the new block height
  -- in our ethereum contract. so create the call.

  blockHash <- bytes . unHex <$> queryBitcoin "getblockhash" [height]
  let notariseCallData = abi "notarise(uint,bytes32,bytes)"
                             (height, blockHash :: Bytes 32, "" :: ByteString)
      notariseTarget = notarisationsContract
      --proxyParams = (unEthGateway gateway, height, notariseCallData)
      -- If get wrong sig or out of order, try below
      -- Maybe sig needs to be normalized
      proxyParams = (unEthGateway gateway, height, abi "getAdmin()" ())
      sighash = ethMakeProxySigMessage proxyParams

  m <- ethCallABI (unEthGateway gateway) "getProxyMessage(address,uint256,bytes)" proxyParams
  liftIO $ do
    print sighash
    print $ toHex $ unBytes $ (m :: Bytes 32)


  -- Ok now we have all the parameters together, we need to collect sigs and get the tx

  runConsensus cparams 'a' $ do
    {- The trick is, that during this whole block inside runConsensus,
       each step will stay open until the end so that lagging nodes can
       join in late. -}

    run $ logDebug "Step 1: Collect sigs"
    sigBallots <- stepWithTopic sighash (collectThreshold threshold) ()

    let proxyCallData = ethMakeProxyCallData proxyParams (bSig <$> unInventory sigBallots)

    run $ logDebug "Step 2: Get proposed transaction"
    txProposed <- propose $ run $ ethMakeTransaction (unEthGateway gateway) proxyCallData

    let Just sig = _sig txProposed
    liftIO $ print $ "TX Lower S: " ++ show (isLowerS sig)

    -- TODO: verifications on proposed tx

    run $ logDebug "Step 3: confirm proposal"
    _ <- step collectMajority ()

    run $ logDebug "Step 4: Submitting transaction"

    run $ liftIO $ print $ recoverFrom txProposed

    receipt <- run $ postTransactionSync txProposed
    liftIO $ print receipt


getBlockRange :: NotariserConfig -> Zeno EthNotariser (Integer, Integer)
getBlockRange NotariserConfig{..} = do
  mlastNota <- getLastNotarisationOnEth
  end <- getKmdProposeHeight 10
  case mlastNota of
       Nothing -> do
         logInfo "No prior notarisations found"
         pure (0, end)
       Just (height, _, _) -> do
         let start = fromIntegral height + 1
         pure (start, end)
  where
  getLastNotarisationOnEth = do
    r <- ethCallABI notarisationsContract "getLastNotarisation()" ()
    pure $
      case r of
        (0, _, _) -> Nothing
        o         -> Just (o :: (Integer, Bytes 32, ByteString))

getKmdProposeHeight :: Has BitcoinConfig r => Integer -> Zeno r Integer
getKmdProposeHeight n = do
  height <- bitcoinGetHeight
  pure $ height - mod height n

toEthIdent :: SecKey -> EthIdent
toEthIdent sk = EthIdent sk $ pubKeyAddr $ derivePubKey sk

