module Contrib.Cardano where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array (catMaybes)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..), note)
import Data.Foldable (class Foldable, fold)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits as String.CodeUnits
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (class Unfoldable)
import Foreign.Object (Object)
import Foreign.Object as Object
import HexString (Hex, hexToString, stringToUtf8Hex, utf8HexToString)
import HexString as HexString

-- blake2b-224

newtype Blake224Hex = Blake224Hex Hex

derive instance Eq Blake224Hex
derive instance Ord Blake224Hex
derive newtype instance Show Blake224Hex

blake224Hex :: Hex -> Maybe Blake224Hex
blake224Hex hex = if String.CodeUnits.length (hexToString hex) == 56
  then Just $ Blake224Hex hex
  else Nothing

newtype PolicyId = PolicyId Blake224Hex
derive instance Eq PolicyId
derive instance Ord PolicyId
derive newtype instance Show PolicyId

instance EncodeJson PolicyId where
  encodeJson (PolicyId (Blake224Hex hex)) = encodeJson $ hexToString hex

instance DecodeJson PolicyId where
  decodeJson json = do
    str <- decodeJson json
    case policyIdFromHexString str of
      Just policyId -> pure policyId
      Nothing -> Left $ TypeMismatch "PolicyId must be a 32-byte hex string"

policyIdToHexString :: PolicyId -> String
policyIdToHexString (PolicyId (Blake224Hex hex)) = hexToString hex

policyIdFromHexString :: String -> Maybe PolicyId
policyIdFromHexString str = do
  h <- HexString.hex str
  PolicyId <$> blake224Hex h

newtype Quantity = Quantity BigInt
derive instance Eq Quantity
derive instance Ord Quantity
derive instance Newtype Quantity _
derive newtype instance Show Quantity
derive newtype instance EncodeJson Quantity
derive newtype instance DecodeJson Quantity

instance Semigroup Quantity where
  append (Quantity a) (Quantity b) = Quantity (a + b)

instance Monoid Quantity where
  mempty = Quantity zero

lovelaceToQuantity :: Lovelace -> Quantity
lovelaceToQuantity (Lovelace x) = Quantity x

quantityToLovelace :: Quantity -> Lovelace
quantityToLovelace (Quantity x) = Lovelace x

-- Migrate to `(AssetName Hex)`
newtype AssetName = AssetName Hex
derive instance Eq AssetName
derive instance Ord AssetName
derive instance Newtype AssetName _
derive newtype instance EncodeJson AssetName
derive newtype instance DecodeJson AssetName
derive newtype instance Show AssetName

-- Should we validate the length of the hex string?
assetNameFromString :: String -> AssetName
assetNameFromString str = AssetName $ stringToUtf8Hex str

assetNameToString :: AssetName -> Maybe String
assetNameToString (AssetName hex) = utf8HexToString hex

assetNameFromHexString :: String -> Maybe AssetName
assetNameFromHexString str = AssetName <$> HexString.hex str

assetNameToHexString :: AssetName -> String
assetNameToHexString (AssetName hex) = hexToString hex

data NonAdaAssetId = NonAdaAssetId PolicyId AssetName

data AssetId = AdaAssetId | AssetId PolicyId AssetName
derive instance Eq AssetId
derive instance Ord AssetId
derive instance Generic AssetId _
instance Show AssetId where
  show = genericShow

assetIdToString :: AssetId -> String
assetIdToString AdaAssetId = ""
assetIdToString (AssetId policyId _) = policyIdToHexString policyId

newtype Value = Value (Map AssetId Quantity)

derive newtype instance Semigroup Value
derive newtype instance Monoid Value

derive newtype instance Eq Value
derive newtype instance Show Value

-- We encode Big Ints as strings because that is convention by multiplatformlib.
instance EncodeJson Value where
  encodeJson v = do
    let
      objFromMap :: forall a. Map String a -> Object a
      objFromMap m = do
        let (arr :: Array _) = Map.toUnfoldable m
        Object.fromFoldable arr

      obj :: Object (Object String)
      obj = objFromMap $ (map (objFromMap <<< map BigInt.toString) (valueToNestedMaps v))
    encodeJson obj

instance DecodeJson Value where
  decodeJson json = do
    obj :: Object (Object String) <- decodeJson json
    obj' <- for obj \m -> for m \s -> do
        case BigInt.fromString s of
          Just x -> pure x
          Nothing -> Left $ TypeMismatch "Value must be a map of maps of strings representing integers"

    let
      mapFromObj :: forall a. Object a -> Map String a
      mapFromObj o = do
        let (arr :: Array _) = Object.toUnfoldable o
        Map.fromFoldable arr

      m :: Map String (Map String BigInt)
      m = mapFromObj $ map mapFromObj obj'
    lmap TypeMismatch $ valueFromNestedMaps m

valueFromNestedMaps :: Map String (Map String BigInt) -> Either String Value
valueFromNestedMaps m = do
  let
    policyId2AssetMap :: Array (String /\ Array (String /\ BigInt))
    policyId2AssetMap = Map.toUnfoldable $ map Map.toUnfoldable m

  Value <<< Map.fromFoldable <<< fold <$> for policyId2AssetMap case _ of
    ("" /\ ["" /\ quantity]) -> pure [AdaAssetId /\ Quantity quantity]
    ("" /\ _) -> Left "Only \"\" is allowed as token name for ADA."
    (policyIdStr /\ assetName2quantity) -> do
      case policyIdFromHexString policyIdStr of
        Nothing -> Left $ "Invalid policy ID: " <> policyIdStr
        Just policyId -> for assetName2quantity \(assetNameStr /\ quantity) -> do
          -- We take non hex string...
          assetName <- note ("Invalid asset name - expecting hex: " <> assetNameStr) $
            assetNameFromHexString assetNameStr
          pure $ AssetId policyId assetName /\ Quantity quantity

valueToNestedMaps :: Value -> Map String (Map String BigInt)
valueToNestedMaps (Value m) = Map.fromFoldableWith (Map.unionWith (+)) $ (Map.toUnfoldable m :: Array _) <#> \(assetId /\ (Quantity quantity)) ->
  case assetId of
    AdaAssetId -> "" /\ Map.singleton "" quantity
    AssetId policyId assetName ->
      -- .. so should output non hex string
      policyIdToHexString policyId /\ Map.singleton (assetNameToHexString assetName) quantity

valueFromFoldable :: forall f. Foldable f => f (AssetId /\ Quantity) -> Value
valueFromFoldable = Value <<< Map.fromFoldable

valueToUnfoldable :: forall f. Unfoldable f => Value -> f (AssetId /\ Quantity)
valueToUnfoldable (Value m) = Map.toUnfoldable m

valueAssetIds :: Value -> Array AssetId
valueAssetIds v = valueToUnfoldable v <#> \(a /\ _) -> a

selectAsset :: Value -> AssetId -> Quantity
selectAsset (Value m) a = fromMaybe mempty $ Map.lookup a m

subtractValues :: Value -> Value -> Value
subtractValues (Value a) (Value b) = do
  let
    -- Quantity should not be negative - we allow this just for a moment ;-)
    sub (Quantity a') (Quantity b') = Quantity (a' - b')
  Value $ Map.filter (_ > mempty) $ Map.unionWith sub a b

newtype NonAdaAssets = NonAdaAssets (Map (PolicyId /\ AssetName) Quantity)
derive instance Eq NonAdaAssets
derive instance Ord NonAdaAssets
derive instance Newtype NonAdaAssets _
derive newtype instance Semigroup NonAdaAssets
derive newtype instance Monoid NonAdaAssets

instance EncodeJson NonAdaAssets where
  encodeJson (NonAdaAssets m) = do
    let
      arr = Array.sort $ Map.toUnfoldable m
    encodeJson $ arr <#> \((policyId /\ assetName) /\ quantity) ->
      [ encodeJson policyId
      , encodeJson assetName
      , encodeJson quantity
      ]

instance DecodeJson NonAdaAssets where
  decodeJson json = do
    (arr :: Array (Array Json)) <- decodeJson json
    m <- Map.fromFoldable <$> for arr case _ of
      [policyIdJson, assetNameJson, quantityJson] -> do
        policyId <- decodeJson policyIdJson
        assetName <- decodeJson assetNameJson
        quantity <- decodeJson quantityJson
        pure ((policyId /\ assetName) /\ quantity)
      _ -> Left $ TypeMismatch "NonAdaAssets must be an array of [policyId, assetName, quantity]"
    pure $ NonAdaAssets m

nonAdaAssets :: Value -> NonAdaAssets
nonAdaAssets (Value m) = NonAdaAssets do
  let
    elems :: Array (AssetId /\ Quantity)
    elems = Map.toUnfoldable m

  Map.fromFoldable $ catMaybes $ elems <#> \(a /\ q) -> case a of
    AdaAssetId -> Nothing
    AssetId cs tn -> Just ((cs /\ tn) /\ q)

newtype Lovelace = Lovelace BigInt
derive instance Eq Lovelace
derive instance Ord Lovelace

instance Semigroup Lovelace where
  append (Lovelace a) (Lovelace b) = Lovelace (a + b)

instance Monoid Lovelace where
  mempty = Lovelace zero

selectLovelace :: Value -> Lovelace
selectLovelace = quantityToLovelace <<< flip selectAsset AdaAssetId

lovelaceToValue :: Lovelace -> Value
lovelaceToValue = Value <<< Map.singleton AdaAssetId <<< lovelaceToQuantity

isLovelaceOnly :: Value -> Boolean
isLovelaceOnly (Value m) = Map.size m == 1 && Map.member AdaAssetId m

lovelaceFromInt :: Int -> Lovelace
lovelaceFromInt = Lovelace <<< BigInt.fromInt

lovelaceToString :: Lovelace -> String
lovelaceToString (Lovelace x) = BigInt.toString x

-- Haskell version:
--      TxOutDatumNone   :: TxOutDatum ctx era
-- 
--      -- | A transaction output that only specifies the hash of the datum, but
--      -- not the full datum value.
--      --
--      TxOutDatumHash   :: ScriptDataSupportedInEra era
--                       -> Hash ScriptData
--                       -> TxOutDatum ctx era
-- 
--      -- | A transaction output that specifies the whole datum value. This can
--      -- only be used in the context of the transaction body, and does not occur
--      -- in the UTxO. The UTxO only contains the datum hash.
--      --
--      TxOutDatumInTx'  :: ScriptDataSupportedInEra era
--                       -> Hash ScriptData
--                       -> ScriptData
--                       -> TxOutDatum CtxTx era
-- 
--      -- | A transaction output that specifies the whole datum instead of the
--      -- datum hash. Note that the datum map will not be updated with this datum,
--      -- it only exists at the transaction output.
--      --
--      TxOutDatumInline :: ReferenceTxInsScriptsInlineDatumsSupportedInEra era
--                       -> ScriptData
--                       -> TxOutDatum ctx era

-- data ReferenceScript
-- 
-- newtype DatumHash = DatumHash String
-- 
-- data TxDatum
--   = TxOutDatumNone
--   | TxOutDatumHash DatumHash
--   | TxOutDatumInTx' DatumHash String
--   | TxOutDatumInline ReferenceScript String
-- 
-- data TxOut = TxOut
--   { address: Bech32
--   , value: Value
--   , datum: Maybe TxDatum
--   , referenceScript: Maybe ReferenceScript
--   }

