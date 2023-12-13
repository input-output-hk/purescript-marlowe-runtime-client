module Contrib.Fetch where

import Prelude

import Contrib.Data.Variant (inj')
import Contrib.Effect.Exception (errorToJson)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Except.Checked (ExceptV)
import Control.Promise as Promise
import Data.Argonaut (Json, encodeJson)
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Fetch.Core as Core
import Fetch.Core.Headers as Headers
import Fetch.Core.Request as CoreRequest
import Fetch.Internal.Request (class ToCoreRequestOptions, HighlevelRequestOptions, new)
import Fetch.Internal.Request as Request
import Fetch.Internal.Response (Response)
import Fetch.Internal.Response as Response
import Foreign.Object as Object
import Prim.Row (class Union)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type StatusCode = Int

data FetchError
  = InvalidStatusCode Response
  | FetchError Error

responseToJson :: Response -> Json
responseToJson response = encodeJson
  { headers: Object.fromFoldable $ Headers.toArray $ response.headers
  , ok: response.ok
  , redirected: response.redirected
  , status: response.status
  , statusText: response.statusText
  , url: response.url
  }

derive instance Generic FetchError _

fetchErrorToJson :: FetchError -> Json
fetchErrorToJson = case _ of
  InvalidStatusCode response -> responseToJson response
  FetchError error -> errorToJson error

instance Show FetchError where
  show (InvalidStatusCode _) = "InvalidStatusCode"
  show (FetchError error) = "FetchError " <> show error

-- For some reason I'm forced to copy the functions bodies in order
-- to avoid inference errors.
fetchEither
  :: forall body input output thruIn thruOut headers err
   . Union input thruIn (HighlevelRequestOptions headers body)
  => Union output thruOut CoreRequest.UnsafeRequestOptions
  => ToCoreRequestOptions input output
  => String
  -> { | input }
  -> Array StatusCode
  -> (FetchError -> err)
  -> Aff (Either err Response)
fetchEither url r allowedStatusCodes handleError = runExceptT do
  let
    fetch = do
      request <- liftEffect $ new url $ Request.convert r
      cResponse <- Promise.toAffE $ Response.promiseToPromise <$> Core.fetch request
      pure $ Response.convert cResponse
  res <- ExceptT $ (Right <$> fetch) `catchError` \err -> do
    pure $ Left $ handleError $ FetchError err

  if res.status `A.elem` allowedStatusCodes then pure res
  else throwError $ handleError $ InvalidStatusCode res

type FetchErrorR r = (fetchError :: FetchError | r)

fetchV
  :: forall body input output thruIn thruOut headers r
   . Union input thruIn (HighlevelRequestOptions headers body)
  => Union output thruOut CoreRequest.UnsafeRequestOptions
  => ToCoreRequestOptions input output
  => String
  -> { | input }
  -> Array StatusCode
  -> ExceptV (FetchErrorR + r) Aff Response
fetchV url r allowedStatusCodes = do
  let
    handleError :: FetchError -> Variant (FetchErrorR + r)
    handleError = inj' @"fetchError"
    fetch = do
      request <- liftEffect $ new url $ Request.convert r
      cResponse <- Promise.toAffE $ Response.promiseToPromise <$> Core.fetch request
      pure $ Response.convert cResponse
  res <- ExceptT $ (Right <$> fetch) `catchError` \err -> do
    pure $ Left $ handleError $ FetchError err

  if res.status `A.elem` allowedStatusCodes then pure res
  else throwError $ handleError $ InvalidStatusCode res

-- For the full safety we should introduce a newtype wrapper for the Response record
jsonBody :: Response -> Aff (Either FetchError Json)
jsonBody response = (Right <<< unsafeCoerce <$> response.json) `catchError` \err -> do
  pure $ Left $ FetchError err

