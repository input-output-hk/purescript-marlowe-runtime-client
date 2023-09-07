module Contrib.Language.Marlowe.Core.V1 where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as String.CodeUnits
import Data.Tuple.Nested ((/\))
import HexString (Hex, hexToString)

compareMarloweJsonKeys :: String -> String -> Ordering
compareMarloweJsonKeys = do
  let
    -- Tiny optimization - let's cache in the closure the ordering
    marloweKeysOrdering :: Map String Int
    marloweKeysOrdering = Map.fromFoldable $ mapWithIndex (flip (/\))
      [
      -- deposit:
        "party"
      , "deposits"
      , "of_token"
      , "into_account"
      -- when:
      , "when"
      , "timeout"
      , "timeout_continuation"
      -- choice:
      , "for_choice"
      , "choose_between"
      -- pay:
      , "pay"
      , "token"
      , "from_account"
      , "to"
      , "then"
      ]
  \a b -> do
    let
      possibleOrdering = do
        aV <- Map.lookup a marloweKeysOrdering
        bV <- Map.lookup b marloweKeysOrdering
        pure $ compare aV bV
    -- Lazily compute the fallback, default ordering
    case possibleOrdering of
      Just ordering -> ordering
      Nothing -> compare a b

newtype Sha256Hex = Sha256Hex Hex

derive instance Eq Sha256Hex
derive instance Ord Sha256Hex

sha256Hex :: Hex -> Maybe Sha256Hex
sha256Hex hex = if String.CodeUnits.length (hexToString hex) == 64
  then Just $ Sha256Hex hex
  else Nothing

newtype CurrencySymbol' = CurrencySymbol' Sha256Hex

derive instance Eq CurrencySymbol'
derive instance Ord CurrencySymbol'

currencySymbolToString :: CurrencySymbol' -> String
currencySymbolToString (CurrencySymbol' (Sha256Hex hex)) = hexToString hex
