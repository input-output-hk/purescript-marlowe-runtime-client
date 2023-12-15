module Test.Main where

import Prelude

import CardanoMultiplatformLib (Bech32)
import CardanoMultiplatformLib.Types (unsafeBech32)
import Contrib.Fetch as Fetch
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (stringify)
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (warn)
import Effect.Exception as Effect.Exception
import JS.Unsafe.Stringify (unsafeStringify)
import Language.Marlowe.Core.V1.Semantics.Types as V1
import Marlowe.Runtime.Contrib.Data.DateTime.Instant (millisecondsFromNow)
import Marlowe.Runtime.Web as R
import Marlowe.Runtime.Web.Client (clientErrorToJson)
import Marlowe.Runtime.Web.Types (PostContractsRequest(..), PostContractsResponseContent(..), SafetyErrorInfo(..), encodeApiError)
import Test.Config as Config
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpecT)

testnetAddress :: V1.Party
testnetAddress = V1.Address "addr_test1qpl2qjntf9p0crd4lqdgdpray4zu8qg8zl9fta2xjqwdpr8llgwss2npxt730zw53lf537hfazx42gwzp7uwzpzmqpyqam4wyt"

testnetAddressBech32 :: Bech32
testnetAddressBech32 = unsafeBech32 "addr_test1qpl2qjntf9p0crd4lqdgdpray4zu8qg8zl9fta2xjqwdpr8llgwss2npxt730zw53lf537hfazx42gwzp7uwzpzmqpyqam4wyt"

mkSingleActionContract :: V1.Action -> Effect V1.Contract
mkSingleActionContract action = do
  let
    oneHour = Milliseconds $ 1000.0 * 60.0 * 60.0
  inOneHour <- millisecondsFromNow oneHour
  pure $ V1.When [ V1.Case action V1.Close ] inOneHour V1.Close

mkContractWithMainnetAddress :: Effect V1.Contract
mkContractWithMainnetAddress = do
  let
    mainnetAddress = V1.Address "addr1qy9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupcvluken35ncjnu0puetf5jvttedkze02d5kf890kquh60sut9jg7"
    lovelace = V1.Token "" ""
    deposit = V1.Deposit mainnetAddress testnetAddress lovelace (V1.Constant $ BigInt.fromInt 1000_000)
  mkSingleActionContract deposit

mkContractWithInvalidCurrencySymbol :: Effect V1.Contract
mkContractWithInvalidCurrencySymbol = do
  let
    invalidToken = V1.Token "f8f8" ""
    deposit = V1.Deposit testnetAddress testnetAddress invalidToken (V1.Constant $ BigInt.fromInt 1000_000)
  mkSingleActionContract deposit

mkContractWithBrokenAddress :: Effect V1.Contract
mkContractWithBrokenAddress = do
  let
    invalidAddress = V1.Address "addr_test1qpl2qjntf9p0crd4lqdgdpra"
    lovelace = V1.Token "" ""
    deposit = V1.Deposit invalidAddress testnetAddress lovelace (V1.Constant $ BigInt.fromInt 1000_000)
  mkSingleActionContract deposit

mkContractWithInvalidAddress :: Effect V1.Contract
mkContractWithInvalidAddress = do
  let
    invalidAddress = V1.Address "addr_test1vqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq3lgle2"
    lovelace = V1.Token "" ""
    deposit = V1.Deposit invalidAddress testnetAddress lovelace (V1.Constant $ BigInt.fromInt 1000_000)
  mkSingleActionContract deposit

mkContractWithTooLongTokenName :: Effect V1.Contract
mkContractWithTooLongTokenName = do
  let
    token = V1.Token
      "00a8d36f0cc7ba944681c8bdddf4fa941940b4ae00edfa9b388a778e"
      "1234567890123456789012345678901234567890123456789012345678901234567890"
    deposit = V1.Deposit testnetAddress testnetAddress token (V1.Constant $ BigInt.fromInt 1000_000)
  mkSingleActionContract deposit

mkContractWithInvalidToken :: Effect V1.Contract
mkContractWithInvalidToken = do
  let
    token = V1.Token
      ""
      "1234567890123456789012345678901234567890123456789012345678901234567890"
    deposit = V1.Deposit testnetAddress testnetAddress token (V1.Constant $ BigInt.fromInt 1000_000)
  mkSingleActionContract deposit

data PredicateResult
  = Satified
  | NotSatisfied String

predicateSatisfied :: PredicateResult -> Boolean
predicateSatisfied Satified = true
predicateSatisfied _ = false

shouldSatisfy'
  :: forall m t
   . MonadThrow Effect.Exception.Error m
  => t
  -> (t -> PredicateResult)
  -> m Unit
shouldSatisfy' v pred = do
  case pred v of
    Satified -> pure unit
    NotSatisfied msg ->
      fail $ "Predicate not satisfied: " <> msg

mainnetContractFailure :: R.Runtime -> Spec Unit
mainnetContractFailure (R.Runtime runtime) = do
  let
    twoAda = V1.Lovelace $ BigInt.fromInt 2000_000
    mkSimpleContractRequest contract minAda = PostContractsRequest
      { metadata: mempty
      , roles: Nothing
      , tags: mempty
      , contract: contract
      , minUTxODeposit: minAda
      , changeAddress: testnetAddressBech32
      , addresses: mempty
      , collateralUTxOs: mempty
      }
    respondedWithFatalSafetyErrors (Right { resource: PostContractsResponseSafetyErrors errors }) = if not <<< any (_.fatal <<< un SafetyErrorInfo) $ errors
      then NotSatisfied "Expected fatal safety errors"
      else Satified
    respondedWithFatalSafetyErrors res@(Right _) = NotSatisfied ("Unexpected valid response: " <> unsafeStringify res)
    respondedWithFatalSafetyErrors (Left x) = do
      let
        err' = show x
      NotSatisfied ("Unexpected error response: " <> err')

    respondedWithBodyEncodingError :: Either (R.ClientError String) _ -> PredicateResult
    respondedWithBodyEncodingError (Right _) = NotSatisfied "Expecting parsing error in the current release"
    respondedWithBodyEncodingError (Left (R.FetchError (Fetch.InvalidBodyEncoding _))) = Satified
    respondedWithBodyEncodingError (Left x) =
      NotSatisfied ("Unexpected error response: " <> stringify (clientErrorToJson encodeApiError x))

    respondedWithServerApiError :: Either (R.ClientError String) _ -> PredicateResult
    respondedWithServerApiError (Right _) = NotSatisfied "Expecting parsing error in the current release"
    respondedWithServerApiError (Left (R.ServerApiError _)) = Satified
    respondedWithServerApiError (Left x) =
      NotSatisfied ("Unexpected error response: " <> stringify (clientErrorToJson encodeApiError x))

  it "trigger fatal error when `mainnet` adddress is used" do
    contract <- liftEffect mkContractWithMainnetAddress
    let
      req = mkSimpleContractRequest contract twoAda
    (res :: Either (R.ClientError String) _) <- R.post' runtime.serverURL runtime.root req
    res `shouldSatisfy'` respondedWithFatalSafetyErrors
  it "triger fatal error when incorrect currency symbol is used" do
    contract <- liftEffect mkContractWithInvalidCurrencySymbol
    let
      req = mkSimpleContractRequest contract twoAda
    (res :: Either (R.ClientError String) _) <- R.post' runtime.serverURL runtime.root req
    warn $ "We expect this to be fixed in the Runtime in the future"
    res `shouldSatisfy'` respondedWithServerApiError
  it "triger fatal error when incorrect address is used" do
    contract <- liftEffect mkContractWithInvalidAddress
    let
      req = mkSimpleContractRequest contract twoAda
    (res :: Either (R.ClientError String) _) <- R.post' runtime.serverURL runtime.root req
    warn $ "We expect this to be fixed in the Runtime in the future"
    res `shouldSatisfy'` respondedWithBodyEncodingError
  it "trigger fatal error when broken address is used" do
    contract <- liftEffect mkContractWithBrokenAddress
    let
      req = mkSimpleContractRequest contract twoAda
    (res :: Either (R.ClientError String) _) <- R.post' runtime.serverURL runtime.root req
    warn $ "We expect this to be fixed in the Runtime in the future"
    res `shouldSatisfy'` respondedWithBodyEncodingError
  it "triger fatal error when token name is too long" do
    contract <- liftEffect mkContractWithTooLongTokenName
    let
      req = mkSimpleContractRequest contract twoAda
    (res :: Either (R.ClientError String) _) <- R.post' runtime.serverURL runtime.root req
    res `shouldSatisfy'` respondedWithFatalSafetyErrors
  it "triger fatal error when invalid token is used" do
    contract <- liftEffect mkContractWithInvalidToken
    let
      req = mkSimpleContractRequest contract twoAda
    (res :: Either (R.ClientError String) _) <- R.post' runtime.serverURL runtime.root req
    res `shouldSatisfy'` respondedWithFatalSafetyErrors

main :: Effect Unit
main = launchAff_ do
  config <- Config.load
  let
    specConfig = { slow: Milliseconds 5000.0, timeout: Just $ Milliseconds 10000.0, exit: false }
  void $ un Identity $ runSpecT specConfig [consoleReporter] do
    describe "Security checks" do
      mainnetContractFailure config.runtime
