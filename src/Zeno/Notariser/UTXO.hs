
module Zeno.Notariser.UTXO where

import Control.Monad.Reader

import Data.Set (Set, (\\), insert, fromList, delete)
import Network.Bitcoin
import Network.Komodo
import Network.ZCash.Sapling
import qualified Haskoin as H

import System.IO.Unsafe

import UnliftIO

import Zeno.Data.Aeson
import Zeno.Prelude
import Zeno.Process
import Zeno.Console


kmdInputAmount :: Word64
kmdInputAmount = 9800


-- TODO: Should be a single "Has Komodo r"
forkMonitorUTXOs :: (Has KomodoIdent r, Has BitcoinConfig r)
                 => Int -> Int -> Zeno r ()
forkMonitorUTXOs minimum nsplit = do
  KomodoIdent{..} <- asks has
  run $ do
    available <- filter viableUtxo <$> komodoUtxos [kmdAddress]

    when (length available < minimum) do
      logInfo $ printf "Creating %i UTXOs of %i" nsplit amount
      makeSplits amount nsplit
      threadDelay $ 5 * 60 * 1000000

    threadDelay $ 30 * 1000000
  where
    amount = kmdInputAmount
    isViable = filter ((==amount) . utxoAmount)
    onError e = do
      logError $ show e
      threadDelay $ 30 * 1000000
    run act = do
      _ <- spawn "monitor UTXOs" $ \_ -> do
        forever $ act `catchAny` onError
      pure ()


makeSplits :: (Has BitcoinConfig r, Has KomodoIdent r) => Word64 -> Int -> Zeno r ()
makeSplits amount nsplits = do
  KomodoIdent{..} <- asks has

  let outs = replicate nsplits (H.PayPK kmdPubKeyI, amount)
      fee = 10000
      total = (sum $ snd <$> outs) + fee
      viableIns =
        reverse . sortOn (\c -> ( utxoConfirmations c * (-1)
                                , utxoTxid c
                                ))
                       . filter (\c -> utxoAmount c > total)
                       . filter utxoSpendable
      buildSplit x =
        let changeSats = utxoAmount x - total
            changeAddr = getAddrHash $ utxoAddress x
            change = if changeSats >= fee
                        then [(H.PayPKHash changeAddr, changeSats)]
                        else []  -- avoid dust
         in saplingFromLegacy <$> H.buildTx [getOutPoint x] (outs ++ change)

      fromSats :: Word64 -> String
      fromSats sats = printf "%f" $ (fromIntegral sats :: Double) / (10e7)
   in do
     utxos <- viableIns <$> komodoUtxos []
     case utxos of
          [] -> do
            logWarn $ "No funds! Need a spendable input of at least " ++ fromSats total ++ " KMD"
          (x:_) -> do
            logDebug $ "Chose input: " ++ show (getOutPoint x)
            case buildSplit x of
                 Left err -> do
                   logError $ "Could not build transaction: " <> err
                 Right mtx -> do
                   signed <- queryBitcoin "signrawtransaction" [mtx]
                   case signed .? "{hex}" :: Maybe SaplingTx of
                     Just tx -> do
                       splitId <- queryBitcoin "sendrawtransaction" [tx]
                       logInfo $ "Sent split tx: " ++ splitId
                     Nothing -> do
                       logError $ "Could not sign transaction: " <> show signed


type HasUtxos r = (Has BitcoinConfig r, Has KomodoIdent r)


waitForUtxo :: HasUtxos r => Zeno r KomodoUtxo
waitForUtxo = do
  getKomodoUtxo >>= \case
    Just u -> pure u
    Nothing -> do
      logWarn "Waiting for UTXOs"
      withUIProc (UIOther "Waiting for UTXOs") do
        fix \f -> do
          threadDelay $ 10 * 1000000
          getKomodoUtxo >>= maybe f pure


allocatedUtxos :: TVar (Set KomodoUtxo)
allocatedUtxos = unsafePerformIO $ newTVarIO mempty


getKomodoUtxo :: HasUtxos r => Zeno r (Maybe KomodoUtxo)
getKomodoUtxo = do
  kmdAddress <- asks $ kmdAddress . has
  unspent <- filter viableUtxo <$> komodoUtxos [kmdAddress]

  mask_ do
    mutxo <- atomically do
      allocated <- readTVar allocatedUtxos
      let available = toList $ fromList unspent \\ allocated
      case prioritise available of
        [] -> pure Nothing
        (u:_) -> do
          writeTVar allocatedUtxos $ insert u allocated
          pure $ Just u

    case mutxo of
      Nothing -> pure Nothing
      Just u -> do
        allocate (pure u) deallocateUtxo
        pure $ Just u
  
  where
  deallocateUtxo u = atomically $ modifyTVar allocatedUtxos (delete u)
  prioritise = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))


viableUtxo :: KomodoUtxo -> Bool
viableUtxo Utxo{..} = utxoAmount == kmdInputAmount && utxoSpendable
