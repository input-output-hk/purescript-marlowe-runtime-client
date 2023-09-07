module Contrib.Data.Array where

import Prelude

import Data.Array (drop, take)

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (take n xs) <> (chunks n $ drop n xs)
