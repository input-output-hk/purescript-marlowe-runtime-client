module Marlowe.Runtime.Web.Streaming
  ( contracts
  , contractsTransactions
  , contractsStates
  , contractsWithTransactions
  , mkContractsWithTransactions
  , ContractEvent
  , ContractMap
  , ContractStream(..)
  , ContractStateStream(..)
  , ContractStateEvent(..)
  , ContractStateMap(..)
  , ContractTransactionsEvent
  , ContractTransactionsMap
  , ContractTransactionsStream(..)
  , ContractWithTransactionsEvent(..)
  , ContractWithTransactionsMap
  , ContractWithTransactions
  , ContractWithTransactionsStream(..)
  , MaxPages(..)
  , PollingInterval(..)
  , RequestInterval(..)
  , TxHeaderWithEndpoint(..)
  ) where

import Prelude

import Contrib.Data.Map (New(..), Old(..), additions, deletions, fromFoldableBy, updates) as Map
import Contrib.Effect as Effect
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError)
import Control.Monad.Rec.Class (forever)
import Control.Parallel (parSequence)
import Data.Filterable (filter)
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Map (catMaybes, empty, filter, insert, lookup, union) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype as Newtype
import Data.Traversable (for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds, delay)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription (Listener)
import Halogen.Subscription as Subscription
import Marlowe.Runtime.Web.Client (foldMapMContractPages, getPages', getResource')
import Marlowe.Runtime.Web.Types (class QueryParams, ContractEndpoint, ContractId, ContractState, ContractsEndpoint, GetContractResponse, GetContractsResponse, ServerURL, TransactionEndpoint, TransactionsEndpoint, TxHeader, api, txOutRefToString)
import Unsafe.Coerce (unsafeCoerce)

-- | API CAUTION: We update the state in chunks but send the events one by one. This means that
-- | the event handler can see some state changes (in `getLiveState`) before it receives some notifications.
-- | `getState` provides a consistent but possibly blocking view of the state.

data ContractEvent
  = Addition GetContractsResponse
  | Deletion GetContractsResponse
  | Update { old :: GetContractsResponse, new :: GetContractsResponse }

contractsById :: Array GetContractsResponse -> Map ContractId GetContractsResponse
contractsById = Map.fromFoldableBy $ _.contractId <<< Newtype.unwrap <<< _.resource

newtype RequestInterval = RequestInterval Milliseconds

newtype PollingInterval = PollingInterval Milliseconds

-- | TODO: Provide nicer types.
type ContractMap = Map ContractId GetContractsResponse

newtype ContractStream = ContractStream
  { emitter :: Subscription.Emitter ContractEvent
  , getLiveState :: Effect ContractMap
  , getState :: Aff ContractMap
  , start :: Aff Unit
  }

newtype MaxPages = MaxPages Int

-- | FIXME: take closer at error handling woudn't this component break in the case of network error?
-- | TODO: we should return `Aff` or fiber and allow more flexible "threading" management.
-- Use constraint at the end: `Warn (Text "pushPullContractsStreams is deprecated, use web socket based implementation instead!")`
contracts
  :: forall params
   . QueryParams ContractsEndpoint params
  => PollingInterval
  -> RequestInterval
  -> params
  -> (GetContractsResponse -> Boolean)
  -> Maybe MaxPages
  -> ServerURL
  -> Aff ContractStream
contracts (PollingInterval pollingInterval) (RequestInterval requestInterval) params filterContracts possibleMaxPages serverUrl = do
  contractsRef <- liftEffect $ Ref.new Map.empty
  pageNumberRef <- liftEffect $ Ref.new 0
  contractsAVar <- AVar.empty

  { emitter, listener } <- liftEffect Subscription.create

  let
    range = Nothing

  let
    start = forever do
      liftEffect $ Ref.write 0 pageNumberRef
      void $ AVar.tryTake contractsAVar
      previousContracts <- liftEffect $ Ref.read contractsRef
      nextContracts :: Map ContractId GetContractsResponse <-
        map contractsById $ Effect.liftEither =<< foldMapMContractPages @String serverUrl api params range \pageContracts -> do
          let
            pageContracts' = filter filterContracts pageContracts
          liftEffect do
            let
              cs :: Map ContractId GetContractsResponse
              cs = contractsById pageContracts'
            Ref.modify_ (Map.union cs) contractsRef
            for_ (Map.additions (Map.Old previousContracts) (Map.New cs)) $ Subscription.notify listener <<< Addition
            for_ (Map.updates (Map.Old previousContracts) (Map.New cs)) $ Subscription.notify listener <<< Update
          pageNumber <- liftEffect $ Ref.modify (add 1) pageNumberRef
          delay requestInterval
          pure
            { result: pageContracts'
            , stopFetching: case possibleMaxPages of
                Nothing -> false
                Just (MaxPages maxPages) -> pageNumber >= maxPages
            }
      liftEffect do
        Ref.write nextContracts contractsRef
        for_ (Map.deletions (Map.Old previousContracts) (Map.New nextContracts)) $ Subscription.notify listener <<< Deletion
      AVar.put nextContracts contractsAVar
      delay pollingInterval

  pure $ ContractStream
    { emitter
    , getLiveState: Ref.read contractsRef
    , getState: AVar.read contractsAVar
    , start
    }

-- | The input set of endpoints which should be used for quering transactions.
type TransactionsEndpointsSource = Map ContractId TransactionsEndpoint

-- | The resuling set of txs per contract.
type ContractTransactionsMap = Map ContractId (Array TxHeaderWithEndpoint)

type ContractTransactionsEvent = ContractId
  /\ { new :: Array TxHeaderWithEndpoint, old :: Maybe (Array TxHeaderWithEndpoint) }

newtype ContractTransactionsStream = ContractTransactionsStream
  { emitter :: Subscription.Emitter ContractTransactionsEvent
  , getLiveState :: Effect ContractTransactionsMap
  , getState :: Aff ContractTransactionsMap
  , sync :: ContractId -> Aff Unit
  , start :: Aff Unit
  }

-- | FIXME: take closer at error handling woudn't this component break in the case of network error?
contractsTransactions
  :: PollingInterval
  -> RequestInterval
  -> Aff TransactionsEndpointsSource
  -> ServerURL
  -> Aff ContractTransactionsStream
contractsTransactions (PollingInterval pollingInterval) requestInterval getEndpoints serverUrl = do
  transactionsRef <- liftEffect $ Ref.new Map.empty
  transactionsAVar <- AVar.empty

  { emitter, listener } <- liftEffect Subscription.create

  let
    start = forever do
      void $ AVar.tryTake transactionsAVar
      endpoints <- getEndpoints
      newTransactions <- fetchContractsTransactions endpoints listener requestInterval serverUrl transactionsRef

      AVar.put newTransactions transactionsAVar
      delay pollingInterval

    sync contractId = do
      let
        endpoint = unsafeCoerce $ "/contracts/" <> txOutRefToString contractId <> "/transactions"
      void $ fetchContractTransactions contractId endpoint listener serverUrl transactionsRef

  pure $ ContractTransactionsStream
    { emitter
    , getLiveState: Ref.read transactionsRef
    , getState: AVar.read transactionsAVar
    , start
    , sync
    }

fetchContractTransactions
  :: ContractId
  -> TransactionsEndpoint
  -> Listener ContractTransactionsEvent
  -> ServerURL
  -> Ref.Ref ContractTransactionsMap
  -> Aff (Maybe (Array TxHeaderWithEndpoint))
fetchContractTransactions contractId transactionEndpoint listener serverUrl transactionsRef = do
  let
    action = do
      (txHeaders :: Array { resource :: TxHeader, links :: { transaction :: TransactionEndpoint } }) <- do
        pages <- getPages' @String serverUrl transactionEndpoint {} Nothing >>= Effect.liftEither
        pure $ foldMap _.page pages

      previousState <- liftEffect $ Ref.read transactionsRef

      let
        oldTransactions = Map.lookup contractId previousState
        newTransactions = txHeaders <#> \{ resource, links: { transaction: transactionEndpoint' } } ->
          resource /\ transactionEndpoint'
        change =
          if Just (map fst newTransactions) == (map fst <$> oldTransactions) then
            Nothing
          else
            Just { old: oldTransactions, new: newTransactions }

      liftEffect do
        case change of
          Just c -> Subscription.notify listener (contractId /\ c)
          Nothing -> pure unit
        Ref.modify_ (\s -> Map.insert contractId newTransactions s) transactionsRef

      pure $ Just newTransactions
  action `catchError` \_ -> do
    pure Nothing

fetchContractsTransactions
  :: TransactionsEndpointsSource
  -> Listener ContractTransactionsEvent
  -> RequestInterval
  -> ServerURL
  -> Ref.Ref ContractTransactionsMap
  -> Aff ContractTransactionsMap
fetchContractsTransactions endpoints listener (RequestInterval requestInterval) serverUrl transactionsRef = do
  map Map.catMaybes $ forWithIndex endpoints \contractId endpoint -> do
    delay requestInterval
    fetchContractTransactions contractId endpoint listener serverUrl transactionsRef

-- | The input set of endpoints which should be used for quering transactions.
type ContractEndpointsSource = Map ContractId ContractEndpoint

-- | The resuling set of txs per contract.
type ContractStateMap = Map ContractId ContractState

type ContractStateEvent = ContractId /\ { new :: ContractState, old :: Maybe ContractState }

newtype ContractStateStream = ContractStateStream
  { emitter :: Subscription.Emitter ContractStateEvent
  , getLiveState :: Effect ContractStateMap
  , getState :: Aff ContractStateMap
  , start :: Aff Unit
  , sync :: ContractId -> Aff Unit
  }

-- | FIXME: the same as above - take closer at error handling woudn't this component break in the case of network error?
contractsStates
  :: PollingInterval
  -> RequestInterval
  -> Aff ContractEndpointsSource
  -> ServerURL
  -> Aff ContractStateStream
contractsStates (PollingInterval pollingInterval) requestInterval getEndpoints serverUrl = do
  stateRef <- liftEffect $ Ref.new Map.empty
  stateAVar <- AVar.empty

  { emitter, listener } <- liftEffect Subscription.create

  let
    start = forever do
      void $ AVar.tryTake stateAVar
      endpoints <- getEndpoints
      newState <- fetchContractsStates endpoints listener requestInterval serverUrl stateRef

      AVar.put newState stateAVar

      delay pollingInterval

    sync contractId = do
      let
        endpoint = unsafeCoerce $ "/contracts/" <> txOutRefToString contractId
      void $ fetchContractState contractId endpoint listener serverUrl stateRef

  pure $ ContractStateStream
    { emitter
    , getLiveState: Ref.read stateRef
    , getState: AVar.read stateAVar
    , sync
    , start
    }

fetchContractState
  :: ContractId
  -> ContractEndpoint
  -> Listener ContractStateEvent
  -> ServerURL
  -> Ref.Ref ContractStateMap
  -> Aff (Maybe ContractState)
fetchContractState contractId endpoint listener serverUrl stateRef = do
  let
    action = do
      previousState <- liftEffect $ Ref.read stateRef
      newContractState <- getResource' @String serverUrl endpoint {} {} >>= Effect.liftEither <#> _.payload.resource
      let
        oldContractState = Map.lookup contractId previousState
        change =
          if oldContractState /= Just newContractState then Nothing
          else pure { old: oldContractState, new: newContractState }

      liftEffect do
        case change of
          Just c -> Subscription.notify listener (contractId /\ c)
          Nothing -> pure unit
        Ref.modify_ (\s -> Map.insert contractId newContractState s) stateRef

      pure $ Just newContractState
  action `catchError` \_ -> do
    pure Nothing

fetchContractsStates
  :: ContractEndpointsSource
  -> Listener ContractStateEvent
  -> RequestInterval
  -> ServerURL
  -> Ref.Ref ContractStateMap
  -> Aff ContractStateMap
fetchContractsStates endpoints listener (RequestInterval requestInterval) serverUrl stateRef = do
  map Map.catMaybes $ forWithIndex endpoints \contractId endpoint -> do
    delay requestInterval
    fetchContractState contractId endpoint listener serverUrl stateRef

type TxHeaderWithEndpoint = TxHeader /\ TransactionEndpoint

type ContractWithTransactions =
  { contract :: GetContractsResponse
  -- | This fetch is done for every contract
  -- | but we don't want to wait with the updates
  -- | until all the states are fetched.
  , contractState :: Maybe GetContractResponse
  , transactions :: Array TxHeaderWithEndpoint
  }

type ContractWithTransactionsMap = Map ContractId ContractWithTransactions

data ContractWithTransactionsEvent
  = ContractEvent ContractEvent
  | ContractStateEvent ContractStateEvent
  | ContractTransactionsEvent ContractTransactionsEvent

newtype ContractWithTransactionsStream = ContractWithTransactionsStream
  { emitter :: Subscription.Emitter ContractWithTransactionsEvent
  , getLiveState :: Effect ContractWithTransactionsMap
  , getState :: Aff ContractWithTransactionsMap
  , sync :: ContractId -> Aff Unit
  , start :: Aff Unit
  }

contractsWithTransactions :: ContractStream -> ContractStateStream -> ContractTransactionsStream -> ContractWithTransactionsStream
contractsWithTransactions (ContractStream contractStream) (ContractStateStream contractStateStream) (ContractTransactionsStream contractTransactionsStream) = do
  let
    getLiveState = do
      contractMap <- contractStream.getLiveState
      contractTransactionsMap <- contractTransactionsStream.getLiveState
      contractStateMap <- contractStateStream.getLiveState

      forWithIndex contractMap \contractId contract -> do
        let
          transactions = fromMaybe [] $ Map.lookup contractId contractTransactionsMap
          contractState = Map.lookup contractId contractStateMap
        pure { contract, contractState, transactions }

    getState = do
      contractMap <- contractStream.getState
      contractStateMap <- contractStateStream.getState
      contractTransactionsMap <- contractTransactionsStream.getState

      forWithIndex contractMap \contractId contract -> do
        let
          transactions = fromMaybe [] $ Map.lookup contractId contractTransactionsMap
          contractState = Map.lookup contractId contractStateMap
        pure { contract, contractState, transactions }

    emitter = (ContractEvent <$> contractStream.emitter)
      <|> (ContractTransactionsEvent <$> contractTransactionsStream.emitter)
      <|> (ContractStateEvent <$> contractStateStream.emitter)

    sync contractId = do
      contractStateStream.sync contractId
      contractTransactionsStream.sync contractId

    start = map (const unit) $ parSequence
      [ void $ contractStateStream.start
      , void $ contractTransactionsStream.start
      , void $ contractStream.start
      ]

  ContractWithTransactionsStream { emitter, getLiveState, getState, sync, start }

mkContractsWithTransactions
  :: forall params
   . QueryParams ContractsEndpoint params
  => PollingInterval
  -> RequestInterval
  -> params
  -> (GetContractsResponse -> Boolean)
  -> Maybe MaxPages
  -> ServerURL
  -> Aff ContractWithTransactionsStream
mkContractsWithTransactions pollingInterval requestInterval params filterContracts possibleMaxPages serverUrl = do
  contractStream@(ContractStream { getState }) <- contracts pollingInterval requestInterval params filterContracts possibleMaxPages serverUrl
  let
    transactionEndpointsSource = Map.catMaybes <<< map (_.links.transactions) <<< Map.filter filterContracts <$> getState
    contractEndpointsSource = map (_.links.contract) <<< Map.filter filterContracts <$> getState

  contractStateStream <- contractsStates
    pollingInterval
    requestInterval
    contractEndpointsSource
    serverUrl

  contractTransactionsStream <- contractsTransactions
    pollingInterval
    requestInterval
    transactionEndpointsSource
    serverUrl

  pure $ contractsWithTransactions contractStream contractStateStream contractTransactionsStream
