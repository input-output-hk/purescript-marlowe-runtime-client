module Contrib.Data.Variant where

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj, on)
import Prim.Row as Row
import Type.Prelude (Proxy(..))

inj'
  :: forall @sym a r1 r2
   . Row.Cons sym a r1 r2
  => IsSymbol sym
  => a
  -> Variant r2
inj' = inj (Proxy @sym)


on'
  :: forall @sym a b r1 r2
  . Row.Cons sym a r1 r2
  => IsSymbol sym
  => (a -> b)
  -> (Variant r1 -> b)
  -> Variant r2
  -> b
on' f g = on (Proxy @sym) f g
