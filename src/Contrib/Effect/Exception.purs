module Contrib.Effect.Exception where

import Data.Argonaut (Json, encodeJson)
import Effect.Exception (Error)
import Effect.Exception as Exception

errorToJson :: Error -> Json
errorToJson err = encodeJson
  { name: Exception.name err
  , msg: Exception.message err
  , stack: Exception.stack err
  }
