
module SaplingSigHash where

import Data.Serialize
import Zeno.Prelude
import Zeno.Data.Hex
import qualified Haskoin as H
import Network.Komodo

import Network.ZCash.Sapling
import Network.ZCash.Sapling.Sign

tx :: SaplingTx
Right tx = decode $ unHex "0400008085202f8904d0e4e3486ea970daa1cb3c97216f96ea5ff6257a71ac2d43c7cb7e37d04dbc4b0b0000004847304402207a21d81ef7ae31fdb5c6aa1ef1e255db30da05fbc0a79fbf7594bcade7867b6802202acd7b154f0ff3e422621c99229a93319eb1d8e2559a2f3e1a05ca877bb8fe5801ffffffffadc794c003fb63b4c1fd3e16c58a7056961d39bf73bcd4805d9f6d0910d2e9370200000049483045022100b1fc3878a851565fc7f77f0cd58007181b4dc3572b2922522266d683dd8e3be10220717da620c69756d385e8e1998cea6c540075716ac6248318ac6d62b78f95dda501ffffffffc7eb8403b264e00a26eb12f7da32eaec6cf1e9c5aea45ad3af55bdc682b24ecc0700000049483045022100c228003fc4756ac7e1fd7ba98bfacac7c8510e67a3326ad4a8e5f59a432b420b02202c88749297fb8c425c445127b5b0cf2aaafe219fa0ca9b0cc26744b0674987ee01ffffffffc26d4f623ce94a88c59fbcec3a8386cf6bf5d18ef7111943050174181755045e080000004847304402202fec611cb1633a3e4baf777be0fabcc54bc41b9277efac141f3c6ede5ee2b61f02203dec00147053e6126abff07b5f6a19e1a352e4f3252e7986884b06a2df71f15701ffffffff027a260000000000002321020e46e79a2a8d12b9b5d12c7a91adb4e454edfae43c0a0cb805427d2ac7613fd9ac0000000000000000736a4c7036d359ed61aca65e663911df53e2d21eb39e41cb6c826f7bcbc35baa4312ca00fc300000007be60200000000000000000000000000000000000000000000000000000000545853434c5a330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"


pks :: [H.PubKeyI]
pks = [ "0343c2f41608f9b6af238d1caaa7f2e479fecb1e34969b4031e1a20145f1a6c4ca"
      , "03994871f2b5d1cb25acad1432382b887b2b7dbd20b526c356dcd0561ea42b4cc2"
      , "02b61089cfab5f3db62d78d8de8189dc591d7a4688d0b69204df9572dfeed780c9"
      , "02ad68cd3bf03b4b2442dc72acc0ef9d676f384891609f0fb4af45122f4e52b316"
      ]


getSigHashes :: SaplingTx -> [H.PubKeyI] -> [Bytes32]
getSigHashes stx@(SaplingTx 4 vins vouts _) pks =
  let tx = saplingToLegacy stx
      sigins = [mkSigIn (H.prevOutput i) pk | (i, pk) <- zip vins pks]
      Right shs = sequence [makeSigHashOverwintered komodo i sigin tx | (i, sigin) <- zip [0..] sigins]
   in toFixed <$> shs

dumpSigs stx =
  let Right r = sequence $ H.decodeInputBS komodo . H.scriptInput <$> txIn stx
   in H.txSignature . H.getInputSig . H.getRegularInput <$> r

verifySigs tx pks =
  let sighashes = getSigHashes tx pks
      Just msgs = sequence $ H.msg . fromFixed <$> sighashes
      sigs = dumpSigs tx
   in [H.verifySig (H.pubKeyPoint pk) sig msg | (pk, sig, msg) <- zip3 pks sigs msgs]

  
mkSigIn o pk = H.SigInput (H.PayPK pk) 9850 o H.sigHashAll Nothing
