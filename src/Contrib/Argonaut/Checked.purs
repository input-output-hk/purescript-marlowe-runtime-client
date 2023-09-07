module Contrib.Argonaut.Checked where

import Prelude

import Contrib.Data.EitherV (EitherV)
import Contrib.Data.Variant (inj')
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Type.Row (type (+))

type DecodeErrorReport = JsonDecodeError

type JsonDecodeErrorR r = (jsonDecodeError :: DecodeErrorReport | r)

decodeJsonV :: forall a r. DecodeJson a => Json -> EitherV (JsonDecodeErrorR + r) a
decodeJsonV json = lmap injError $ decodeJson json
  where
    injError err = inj' @"jsonDecodeError" err

toChecked :: forall a r. Either JsonDecodeError a -> EitherV (JsonDecodeErrorR + r) a
toChecked = lmap injError
  where
    injError err = inj' @"jsonDecodeError" err
