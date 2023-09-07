module Contrib.Data.Array.NonEmpty where

import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, cons', drop, fromArray, take)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

takeNonEmpty :: forall a. Int -> NonEmptyArray a -> NonEmptyArray a
takeNonEmpty n arr = case fromArray (take n arr) of
  Just nArr -> nArr
  Nothing -> unsafeCrashWith "Contrib.Data.Array.NonEmpty.takeNonEmptyArray: impossible - `take n arr` of non empty array is empty"

chunks :: forall a. Int -> NonEmptyArray a -> NonEmptyArray (NonEmptyArray a)
chunks n arr = do
  let
    chunk = takeNonEmpty n arr
    rest = drop n arr
  cons' chunk (go rest)
  where
  go [] = []
  go xs = case fromArray (Array.take n xs) of
    Just nArr -> nArr : go (Array.drop n xs)
    Nothing -> []
