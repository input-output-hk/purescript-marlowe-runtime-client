module Contrib.Effect.Aff where

import Prelude

import Contrib.Data.Array (chunks)
import Control.Parallel (parallel, sequential)
import Data.Foldable (fold)
import Data.Traversable (for)
import Effect.Aff (Aff)

newtype MaxChunkSize = MaxChunkSize Int

parAffs :: forall a. MaxChunkSize -> Array (Aff a) -> Aff (Array a)
parAffs (MaxChunkSize chunkSize) affs = do
  let
    affsChunks = chunks chunkSize affs

  fold <$> for affsChunks \affsChunk ->
    sequential $ for affsChunk parallel

