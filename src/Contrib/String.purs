module Contrib.String where

import Prelude

import Data.String as String
import Data.Function.Uncurried (Fn3, runFn3)
import Data.String.CodeUnits as CodeUnits

foreign import padStartWithImpl :: Fn3 Int Char String String

-- Takes code unit as padding character
padStartWith :: Int -> Char -> String -> String
padStartWith n c s = do
  let
    numberOfCodePoints = String.length s
    numberOfCodeUnits  = CodeUnits.length s
  runFn3 padStartWithImpl (n + numberOfCodeUnits - numberOfCodePoints) c s

