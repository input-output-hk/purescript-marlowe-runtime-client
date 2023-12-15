module Test.Config where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Lazy (force)
import Data.Map (fromFoldable) as Map
import Data.Newtype (un)
import Data.String as String
import Data.Validation.Semigroup (V(..))
import Dotenv (loadFile) as DotEnv
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (toUnfoldable) as Object
import JS.Unsafe.Stringify (unsafeStringify)
import Marlowe.Runtime.Web (Runtime, getHealthCheck, runtime)
import Marlowe.Runtime.Web.Types (HealthCheck(..), NetworkId(..), NetworkMagic, ServerURL(..))
import Node.Process (getEnv)
import Polyform as Polyform
import Polyform.Batteries.Env (Env, Validator) as Env
import Polyform.Batteries.Env.Validators (required) as Env
import Polyform.Validator (liftFn, liftFnMEither, lmapValidator, runValidator)

type Config =
  { runtime :: Runtime
  , testnetMagic :: NetworkMagic
  }

type Validator m i o = Polyform.Validator m (Array String) i o

stringifyValidator :: forall errs i o m. Monad m => Env.Validator m errs i o -> Validator m i o
stringifyValidator = lmapValidator $ map \{ key, errors } -> do
  let
    errors' = String.joinWith ", " $ errors <#> _.msg >>> force
  "Errors in " <> key <> ": " <> errors'

validator :: Validator Aff Env.Env Config
validator = stringifyValidator (Env.required "MARLOWE_RUNTIME_URL" (liftFn ServerURL)) >>> liftFnMEither \serverURL -> do
  getHealthCheck serverURL >>= case _ of
    Left err -> pure $ Left [ unsafeStringify err ]
    Right (HealthCheck healthcheckInfo) -> case healthcheckInfo.networkId of
      Mainnet -> pure $ Left [ "Mainnet is not supported" ]
      Testnet testnetMagic ->
        pure $ Right
          { runtime: runtime serverURL
          , testnetMagic
          }

load :: Aff Config
load = do
  void $ DotEnv.loadFile
  env <- liftEffect $ getEnv <#> (Object.toUnfoldable :: _ -> Array _) >>> Map.fromFoldable
  runValidator validator env >>= un V
    >>> case _ of
      Left _ -> do
        throwError $ error "Configuration error. Please verify your environment and .env file."
      Right p -> pure p
