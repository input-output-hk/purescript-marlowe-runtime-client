module Marlowe.Runtime.Registry where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Marlowe.Runtime.Web.Types (TxId(..), TxOutRef(..))

-- Partial clone of marlowe-runtime/src/Language/Marlowe/Runtime/Core/ScriptRegistry.hs

newtype NetworkMagic = NetworkMagic Int
derive instance Eq NetworkMagic
derive instance Ord NetworkMagic

data NetworkId = Mainnet | Testnet NetworkMagic
derive instance Eq NetworkId

instance Ord NetworkId where
  compare Mainnet Mainnet = EQ
  compare Mainnet _ = LT
  compare _ Mainnet = GT
  compare (Testnet (NetworkMagic a)) (Testnet (NetworkMagic b)) =
    compare a b

newtype ReferenceScriptUtxo = ReferenceScriptUtxo TxOutRef
derive instance Eq ReferenceScriptUtxo
derive instance Ord ReferenceScriptUtxo

newtype ScriptHash = ScriptHash String
derive instance Eq ScriptHash
derive instance Ord ScriptHash

newtype MarloweScripts = MarloweScripts
  { marloweScript :: ScriptHash
  , payoutScript :: ScriptHash
  , marloweScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
  , payoutScriptUTxOs :: Map NetworkId ReferenceScriptUtxo
  }
derive instance Eq MarloweScripts
derive instance Ord MarloweScripts

mainnetNetworkId :: NetworkId
mainnetNetworkId = Mainnet

preprodNetworkId :: NetworkId
preprodNetworkId = Testnet $ NetworkMagic 1

previewNetworkId :: NetworkId
previewNetworkId = Testnet $ NetworkMagic 2

currentV1Scripts :: MarloweScripts
currentV1Scripts = MarloweScripts
  { marloweScript: ScriptHash "2ed2631dbb277c84334453c5c437b86325d371f0835a28b910a91a6e"
  , payoutScript: ScriptHash "e165610232235bbbbeff5b998b233daae42979dec92a6722d9cda989"
  , marloweScriptUTxOs: Map.fromFoldable
    [ do
        let
          txOutRef = TxOutRef { txId: TxId "672399f7d551d6e06fda70769f830e4e3783495c6250567c6ae97ecc788ad5a4", txIx: 1 }
        mainnetNetworkId /\ (ReferenceScriptUtxo txOutRef)
    , do
        let
          txOutRef = TxOutRef { txId: TxId "9a8a6f387a3330b4141e1cb019380b9ac5c72151c0abc52aa4266245d3c555cd", txIx: 1 }
        preprodNetworkId /\ ReferenceScriptUtxo txOutRef
    , do
        let
          txOutRef = TxOutRef { txId: TxId "69bfdb7cd911e930bfa073a8c45121e7690939d7680196181731d0dd609ecb73", txIx: 1 }
        previewNetworkId /\ ReferenceScriptUtxo txOutRef
    ]
  , payoutScriptUTxOs: Map.fromFoldable
    [ do
        let
          txOutRef = TxOutRef { txId: TxId "672399f7d551d6e06fda70769f830e4e3783495c6250567c6ae97ecc788ad5a4", txIx: 2 }
        mainnetNetworkId /\ ReferenceScriptUtxo txOutRef
    , do
        let
          txOutRef = TxOutRef { txId: TxId "9a8a6f387a3330b4141e1cb019380b9ac5c72151c0abc52aa4266245d3c555cd", txIx: 2 }
        preprodNetworkId /\ ReferenceScriptUtxo txOutRef
    , do
        let
          txOutRef = TxOutRef { txId: TxId "69bfdb7cd911e930bfa073a8c45121e7690939d7680196181731d0dd609ecb73", txIx: 2 }
        previewNetworkId /\ ReferenceScriptUtxo txOutRef
    ]
  }

