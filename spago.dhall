{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "arrays"
  , "avar"
  , "bifunctors"
  , "cardano-multiplatform-lib"
  , "checked-exceptions"
  , "control"
  , "datetime"
  , "datetime-iso"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "fetch"
  , "fetch-core"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "halogen-subscriptions"
  , "heterogeneous"
  , "http-methods"
  , "indexed-monad"
  , "integers"
  , "js-date"
  , "lists"
  , "marlowe"
  , "maybe"
  , "monad-loops"
  , "newtype"
  , "ordered-collections"
  , "parallel"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor"
  , "record"
  , "refs"
  , "row-joins"
  , "safe-coerce"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "typelevel-eval"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "uri"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
