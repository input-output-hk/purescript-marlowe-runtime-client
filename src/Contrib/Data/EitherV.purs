module Contrib.Data.EitherV where

import Data.Either (Either)
import Data.Variant (Variant)


type EitherV r a = Either (Variant r) a
