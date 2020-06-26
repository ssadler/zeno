
module Zeno.Notariser.UTXO
  ( withKomodoUtxo
  , forkMonitorUTXOs
  , kmdInputAmount
  ) where

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
kmdInputAmount = 9850


-- TODO: Should be a single "Has Komodo r"
forkMonitorUTXOs :: (Has KomodoIdent r, Has BitcoinConfig r)
                 => Int -> Int -> Zeno r ()
forkMonitorUTXOs minimum nsplit = do
  run do
    available <- getSpendableUtxos
    nallocated <- length <$> readMVar allocatedUtxos

    when (length available - nallocated < minimum) do
      logInfo $ printf "Creating %i UTXOs of %i" nsplit amount
      makeSplits available amount nsplit
      threadDelay $ 5 * 60 * 1000000

    threadDelay $ 30 * 1000000
  where
    amount = kmdInputAmount
    isViable = filter ((==amount) . utxoAmount)
    onError e = do
      logError $ show e
      threadDelay $ 30 * 1000000
    run act = do
      spawnNoHandle "monitor UTXOs" do
        forever $ act `catchAny` onError
      pure ()


makeSplits :: (Has BitcoinConfig r, Has KomodoIdent r) => [KomodoUtxo] -> Word64 -> Int -> Zeno r ()
makeSplits available amount nsplits = do
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
     utxos <- viableIns <$> komodoListUnspent []
     case utxos of
          [] -> do
            logWarn $ "No funds! Need a spendable input of at least " ++ fromSats total ++ " KMD"
          (x:_) -> do
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


allocatedUtxos :: MVar (Set KomodoUtxo)
allocatedUtxos = unsafePerformIO $ newMVar mempty

withKomodoUtxo :: HasUtxos r => (KomodoUtxo -> Zeno r a) -> Zeno r a
withKomodoUtxo act = do
  bracket
    waitForUtxo
    (\u -> modifyMVar_ allocatedUtxos $ pure . delete u)
    act

waitForUtxo :: HasUtxos r => Zeno r KomodoUtxo
waitForUtxo = do
  getKomodoUtxo >>= \case
    Just u -> pure u
    Nothing -> do
      logWarn "Waiting for UTXOs"
      sendUI $ UI_Process $ Just $ UIOther "Waiting for UTXOs"
      fix \f -> do
        threadDelay $ 10 * 1000000
        getKomodoUtxo >>= maybe f pure

getKomodoUtxo :: HasUtxos r => Zeno r (Maybe KomodoUtxo)
getKomodoUtxo = do
  unspent <- getSpendableUtxos

  modifyMVar allocatedUtxos \allocated -> do
    let available = toList $ fromList unspent \\ allocated
    case prioritise available of
      [] -> pure (allocated, Nothing)
      (u:_) -> do
        pure (insert u allocated, Just u)
  
  where
  prioritise = reverse . sortOn (\c -> (utxoConfirmations c, utxoTxid c))


getSpendableUtxos :: HasUtxos r => Zeno r [KomodoUtxo]
getSpendableUtxos = do
  KomodoIdent{..} <- asks has
  let spendable Utxo{..} =
        utxoAmount == kmdInputAmount &&
        utxoSpendable &&
        utxoScriptPubKey == Hex (H.encodeOutputBS (H.PayPK kmdPubKeyI))
  komodoListUnspent [kmdAddress] <&> filter spendable
