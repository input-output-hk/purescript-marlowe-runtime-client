module Marlowe.Runtime.Web.Client where

import Prelude

import Contrib.Data.Argonaut (JsonParser)
import Contrib.Data.Argonaut.Generic.Record (class DecodeRecord, DecodeJsonFieldFn)
import Contrib.Fetch (FetchError, StatusCode, fetchEither, jsonBody)
import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, stringify)
import Data.Argonaut.Decode ((.:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, fromRight)
import Data.Foldable (fold, foldMap)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.List as List
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Fetch (RequestMode(..))
import Fetch.Core.Headers (Headers, toArray)
import Foreign.Object (fromHomogeneous)
import Foreign.Object as Object
import Marlowe.Runtime.Web.Types (class EncodeHeaders, class EncodeJsonBody, class QueryParams, class ToResourceLink, ApiError, GetContractsResponse, IndexEndpoint(..), PostMerkleizationRequest, PostMerkleizationResponse, ResourceEndpoint(..), ResourceLink(..), ResourceWithLinks, ResourceWithLinksRow, ServerURL(..), decodeResourceWithLink, encodeHeaders, encodeJsonBody, toQueryParams, toResourceLink)
import Parsing as Parsing
import Prim.Row (class Lacks) as Row
import Record as R
import Safe.Coerce (coerce)
import Type.Prelude (Proxy(..))
import Type.Row.Homogeneous (class Homogeneous) as Row
import URI (RelativeRef(..), URI(..)) as URI
import URI.Extra.QueryPairs (QueryPairs(..), keyFromString, keyToString, valueFromString, valueToString) as URI
import URI.Extra.QueryPairs as URI.QueryPairs
import URI.HostPortPair (HostPortPair) as URI
import URI.HostPortPair as HostPortPair
import URI.URIRef (Fragment, HierPath, Host, Path, Port, RelPath, URIRefOptions, UserInfo) as URI
import URI.URIRef as URIRef

data ClientError err
  = FetchError FetchError
  | ResponseDecodingError JsonDecodeError
  | MerkleizationError
  | ServerApiError (ApiError err)

derive instance Generic (ClientError err) _
instance Show err => Show (ClientError err) where
  show = genericShow

type GetResourceResponse err res = Either (ClientError err) res

allowedStatusCodes :: Array Int
allowedStatusCodes = [ 200, 201, 202, 206, 400, 401, 403, 404, 500 ]

newtype Range = Range String

type DecodeJson a = Json -> Either JsonDecodeError a

decodeResponse :: forall a err. DecodeJson (ApiError err) => JsonParser a -> StatusCode -> Json -> Either (ClientError err) a
decodeResponse parseA = do
  let
    -- decodePossibleResults :: Json -> JsonParser a
    decodePossibleResults json = do
      obj <- decodeJson json
      res <- obj .: "results"
      parseA res
  case _, _ of
    statusCode, json | statusCode >= 200 && statusCode < 300 ->
      lmap ResponseDecodingError (decodePossibleResults json <|> parseA json)
    _, json -> Left $
      either ResponseDecodingError ServerApiError (decodeJson json)

decodeResponse' :: forall a err. DecodeJson a => DecodeJson (ApiError err) => StatusCode -> Json -> Either (ClientError err) a
decodeResponse' = decodeResponse decodeJson

decodeResponseWithLink
  :: forall a err linksRow
   . DecodeRecord (resource :: DecodeJsonFieldFn a) (ResourceWithLinksRow a linksRow)
  => DecodeJson (ApiError err)
  => DecodeJsonFieldFn a
  -> StatusCode
  -> Json
  -> Either (ClientError err) (ResourceWithLinks a linksRow)
decodeResponseWithLink decodeResource statusCode = decodeResponse (decodeResourceWithLink decodeResource) statusCode

getResource
  :: forall a endpoint err extraHeaders params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => Row.Lacks "Access-Control-Request-Headers" extraHeaders
  => Row.Homogeneous ("Access-Control-Request-Headers" :: String | extraHeaders) String
  => QueryParams endpoint params
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> params
  -> { | extraHeaders }
  -> Aff (GetResourceResponse err { headers :: Headers, payload :: a, status :: Int })
getResource (ServerURL serverUrl) endpoint extraParams extraHeaders = do
  let
    ResourceLink path = toResourceLinkWithParams endpoint extraParams
    url = serverUrl <> "/" <> path

    reqHeaders =
      R.insert (Proxy :: Proxy "Access-Control-Request-Headers") "Range, Accept"
        -- $ R.insert (Proxy :: Proxy "Accept") "application/json"
        $ extraHeaders

  runExceptT do
    res@{ status, headers: resHeaders } <- ExceptT $ fetchEither url { headers: reqHeaders, mode: Cors } allowedStatusCodes FetchError
    lift (jsonBody res) >>= decodeResponse' status >>> case _ of
      Left err -> throwError err
      Right payload -> pure { payload, headers: resHeaders, status }

merkleize
  :: forall err
   . DecodeJson (ApiError err)
  => ServerURL
  -> PostMerkleizationRequest
  -> Aff
       ( Either
           (ClientError err)
           { headers :: Headers
           , payload :: PostMerkleizationResponse
           , status :: Int
           }
       )
merkleize (ServerURL serverUrl) req = runExceptT do
  let
    url = serverUrl <> "/contracts/merkleize"
    body = stringify $ encodeJsonBody req

    headers :: { "Accept" :: String, "Content-Type" :: String }
    headers =
      { "Accept": "application/json"
      , "Content-Type": "application/json"
      }

  res@{ status, headers: resHeaders } <- ExceptT $ fetchEither url { method: POST, body, headers } allowedStatusCodes FetchError
  lift (jsonBody res) >>= decodeResponse' status >>> case _ of
    Left err -> throwError err
    Right payload -> pure { payload, headers: resHeaders, status }

getPage
  :: forall a endpoint err params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => QueryParams endpoint params
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> Aff (GetResourceResponse err ({ page :: a, nextRange :: Maybe Range }))
getPage serverUrl path extraParams possibleRange = runExceptT do
  { headers, payload, status } <- ExceptT
    $ case possibleRange of
        Just range -> getResource serverUrl path extraParams { "Range": coerce range }
        Nothing -> getResource serverUrl path extraParams {}
  pure
    { page: payload
    , nextRange:
        if status == 206 then map Range $ lookup (CaseInsensitiveString "Next-Range")
          $ fromFoldable
          $ map (lmap CaseInsensitiveString)
          $ toArray headers
        else Nothing
    }

-- TODO generalize
foldMapMContractPages
  :: forall @err endpoint params
   . ToResourceLink endpoint (Array GetContractsResponse)
  => DecodeJson (ApiError err)
  => QueryParams endpoint params
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> (Array GetContractsResponse -> Aff { result :: Array GetContractsResponse, stopFetching :: Boolean })
  -> Aff (Either (ClientError err) (Array GetContractsResponse))
foldMapMContractPages serverUrl endpoint extraParams start f =
  foldMapMPages' serverUrl endpoint extraParams (f <<< _.page) start

data FoldPageStep = FetchPage (Maybe Range) | StopFetching

foldMapMPages
  :: forall a b endpoint err m params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => MonadAff m
  => Monoid b
  => QueryParams endpoint params
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> params
  -> ({ page :: a, currRange :: Maybe Range } -> m { result :: b, stopFetching :: Boolean })
  -> Maybe Range
  -> m (GetResourceResponse err b)
foldMapMPages serverUrl path extraParams f startRange = do
  bs <- runExceptT $ flip unfoldrM (FetchPage startRange) case _ of
    StopFetching -> pure Nothing
    FetchPage currRange -> do
      { page, nextRange } <- ExceptT $ liftAff $ getPage serverUrl path extraParams currRange
      { result: b, stopFetching } <- lift $ f { page, currRange }
      pure $ Just case nextRange of
        Just _ -> b /\
          if stopFetching then StopFetching
          else (FetchPage nextRange)
        Nothing -> b /\ StopFetching
  pure (fold <$> bs)

toExtraQueryParmas
  :: forall params
   . Row.Homogeneous params (Maybe (Array String))
  => { | params }
  -> Array (String /\ (Maybe String))
toExtraQueryParmas paramsRecord = do
  let
    obj = fromHomogeneous paramsRecord
  k /\ possibleVs <- Object.toUnfoldable obj
  case possibleVs of
    Just vs -> do
      v <- vs
      pure $ k /\ (Just $ v)
    Nothing -> pure $ k /\ Nothing

uriOpts
  :: Record
       ( URI.URIRefOptions URI.UserInfo
           (URI.HostPortPair URI.Host URI.Port)
           URI.Path
           URI.HierPath
           URI.RelPath
           (URI.QueryPairs String String)
           URI.Fragment
       )
uriOpts =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseRelPath: pure
  , printRelPath: identity
  , parseQuery: URI.QueryPairs.parse (URI.keyToString >>> pure) (URI.valueToString >>> pure)
  , printQuery: URI.QueryPairs.print URI.keyFromString URI.valueFromString
  , parseFragment: pure
  , printFragment: identity
  }

toResourceLinkWithParams
  :: forall a endpoint params
   . QueryParams endpoint params
  => ToResourceLink endpoint a
  => endpoint
  -> params
  -> ResourceLink a
toResourceLinkWithParams endpoint params = do
  let
    resourceLink@(ResourceLink uri) = toResourceLink endpoint
    extraQueryParams = toQueryParams (Proxy @endpoint) params
    possibleUriRef = Parsing.runParser uri (URIRef.parser uriOpts)

    mergeQuery possibleOrigQuery = case possibleOrigQuery of
      Nothing -> Just $ URI.QueryPairs extraQueryParams
      Just (URI.QueryPairs origQueryParams) -> Just $ URI.QueryPairs $ origQueryParams <> extraQueryParams

  fromRight resourceLink do
    uriRef <- possibleUriRef
    let
      uriRef' = case uriRef of
        Right (URIRef.RelativeRef relativePart query fragment) -> do
          let
            query' = mergeQuery query
          Right $ URI.RelativeRef relativePart query' fragment
        Left (URI.URI scheme hp query fragment) -> do
          let
            query' = mergeQuery query
          Left $ URI.URI scheme hp query' fragment
    pure $ ResourceLink $ URIRef.print uriOpts uriRef'

getPages
  :: forall a endpoint err m params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => MonadAff m
  => QueryParams endpoint params
  => ToResourceLink endpoint a
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> m (GetResourceResponse err (List { page :: a, currRange :: Maybe Range }))
getPages serverUrl path paramsRecord = do -- params = do
  foldMapMPages serverUrl path paramsRecord (List.singleton >>> \result -> pure { result, stopFetching: false })

getPages'
  :: forall @err endpoint a m params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => MonadAff m
  => ToResourceLink endpoint a
  => QueryParams endpoint params
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> m (GetResourceResponse err (List { page :: a, currRange :: Maybe Range }))
getPages' serverUrl endpoint extraParams = getPages serverUrl endpoint extraParams

getItems
  :: forall err f endpoint b params
   . DecodeJson b
  => DecodeJson (ApiError err)
  => MonadAff f
  => ToResourceLink endpoint b
  => QueryParams endpoint params
  => Monoid b
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> f (Either (ClientError err) b)
getItems serverUrl endpoint extraParams range = do
  getPages serverUrl endpoint extraParams range <#> case _ of
    Left err -> Left err
    Right pages -> Right $ foldMap _.page pages

getItems'
  :: forall @err f endpoint b params
   . MonadAff f
  => DecodeJson b
  => DecodeJson (ApiError err)
  => ToResourceLink endpoint b
  => QueryParams endpoint params
  => Monoid b
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> f (Either (ClientError err) b)
getItems' serverUrl endpoint extraParams range = do
  getPages' serverUrl endpoint extraParams range <#> case _ of
    Left err -> Left err
    Right pages -> Right $ foldMap _.page pages

getResource'
  :: forall @err a extraHeaders params endpoint
   . DecodeJson a
  => DecodeJson (ApiError err)
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Access-Control-Request-Headers" extraHeaders
  => Row.Homogeneous ("Access-Control-Request-Headers" :: String | extraHeaders) String
  => ToResourceLink endpoint a
  => QueryParams endpoint params
  => ServerURL
  -> endpoint
  -> params
  -> Record extraHeaders
  -> Aff (GetResourceResponse err { headers :: Headers, payload :: a, status :: Int })
getResource' serverUrl endpoint extraParams = do
  getResource serverUrl endpoint extraParams

getPage'
  :: forall a endpoint err params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => ToResourceLink endpoint a
  => QueryParams endpoint params
  => ServerURL
  -> endpoint
  -> params
  -> Maybe Range
  -> Aff (GetResourceResponse err ({ page :: a, nextRange :: Maybe Range }))
getPage' serverUrl endpoint extraParams = do
  getPage serverUrl endpoint extraParams

foldMapMPages'
  :: forall a b endpoint err m params
   . DecodeJson a
  => DecodeJson (ApiError err)
  => MonadAff m
  => Monoid b
  => ToResourceLink endpoint a
  => QueryParams endpoint params
  => ServerURL
  -> endpoint
  -> params
  -> ({ currRange :: Maybe Range, page :: a } -> m { result :: b, stopFetching :: Boolean })
  -> Maybe Range
  -> m (Either (ClientError err) b)
foldMapMPages' serverUrl endpoint extraParams = foldMapMPages serverUrl endpoint extraParams

post
  :: forall err postRequest postResponse postResponseLinks getResponse getResponseLinks extraHeaders
   . DecodeJson postResponse
  => DecodeJson (ApiError err)
  => EncodeHeaders postRequest extraHeaders
  => EncodeJsonBody postRequest
  => DecodeRecord (resource :: DecodeJsonFieldFn postResponse) (ResourceWithLinksRow postResponse postResponseLinks)
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks
  -> postRequest
  -> Aff (GetResourceResponse err (ResourceWithLinks postResponse postResponseLinks))
post (ServerURL serverUrl) (IndexEndpoint (ResourceLink path)) req = runExceptT do
  let
    url = serverUrl <> "/" <> path
    body = stringify $ encodeJsonBody req

    headers :: { "Content-Type" :: String | extraHeaders }
    headers =
      -- R.insert (Proxy :: Proxy "Accept") "application/json"
      R.insert (Proxy :: Proxy "Content-Type") "application/json"
        $ (encodeHeaders req :: { | extraHeaders })

  response@{ status } <- ExceptT $ fetchEither url { method: POST, body, headers } allowedStatusCodes FetchError
  (lift (jsonBody response)) >>= decodeResponseWithLink (map decodeJson :: Maybe _ -> Maybe _) status >>> case _ of
    Left err -> throwError err
    Right payload -> pure payload

post'
  :: forall t @err postRequest postResponse postResponseLinks getResponse getResponseLinks extraHeaders
   . Newtype t (IndexEndpoint postRequest postResponse postResponseLinks getResponse getResponseLinks)
  => DecodeJson postResponse
  => DecodeJson (ApiError err)
  => DecodeRecord (resource :: DecodeJsonFieldFn postResponse) (ResourceWithLinksRow postResponse postResponseLinks)
  => EncodeHeaders postRequest extraHeaders
  => EncodeJsonBody postRequest
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> t
  -> postRequest
  -> Aff (Either (ClientError err) (ResourceWithLinks postResponse postResponseLinks))
post' serverUrl endpoint req = do
  let
    endpoint' = unwrap endpoint
  post serverUrl endpoint' req

put
  :: forall links putRequest getResponse extraHeaders
   . EncodeHeaders putRequest extraHeaders
  => EncodeJsonBody putRequest
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> ResourceEndpoint putRequest getResponse links
  -> putRequest
  -> Aff (Either FetchError Unit)
put (ServerURL serverUrl) (ResourceEndpoint (ResourceLink path)) req = runExceptT do
  let
    url = serverUrl <> "/" <> path
    body = stringify $ encodeJsonBody req

    headers :: { "Content-Type" :: String | extraHeaders }
    headers =
      -- R.insert (Proxy :: Proxy "Accept") "application/json"
      R.insert (Proxy :: Proxy "Content-Type") "application/json"
        $ (encodeHeaders req :: { | extraHeaders })
  void $ ExceptT $ fetchEither url { method: PUT, body, headers } allowedStatusCodes identity

put'
  :: forall links putRequest getResponse extraHeaders t
   . EncodeHeaders putRequest extraHeaders
  => EncodeJsonBody putRequest
  => Newtype t (ResourceEndpoint putRequest getResponse links)
  => Row.Homogeneous extraHeaders String
  => Row.Homogeneous ("Content-Type" :: String | extraHeaders) String
  -- => Row.Lacks "Accept" extraHeaders
  => Row.Lacks "Content-Type" extraHeaders
  => ServerURL
  -> t
  -> putRequest
  -> Aff (Either FetchError Unit)
put' serverUrl endpoint req = do
  let
    endpoint' = unwrap endpoint
  put serverUrl endpoint' req
