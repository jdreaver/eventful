module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Test.Hspec

import Eventful.Serializer
import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store with actual event type" $ do
    eventStoreSpec tvarRunner
    sequencedEventStoreSpec tvarGlobalRunner

  describe "TVar memory event store with Dynamic serialized type" $ do
    eventStoreSpec tvarDynamicRunner
    sequencedEventStoreSpec tvarDynamicGlobalRunner

  describe "MonadState memory event store with actual event type" $ do
    eventStoreSpec stateStoreRunner
    sequencedEventStoreSpec stateStoreGlobalRunner

  describe "MonadState embedded memory event store with actual event type" $ do
    eventStoreSpec embeddedStateStoreRunner
    sequencedEventStoreSpec embeddedStateStoreGlobalRunner

tvarRunner :: EventStoreRunner STM
tvarRunner = EventStoreRunner $ \action -> (fst <$> memoryEventStore) >>= atomically . action

tvarGlobalRunner :: GloballyOrderedEventStoreRunner STM
tvarGlobalRunner = GloballyOrderedEventStoreRunner $ \action -> memoryEventStore >>= atomically . uncurry action

tvarDynamicRunner :: EventStoreRunner STM
tvarDynamicRunner = EventStoreRunner $ \action -> (fst <$> makeDynamicTVarStore) >>= atomically . action

tvarDynamicGlobalRunner :: GloballyOrderedEventStoreRunner STM
tvarDynamicGlobalRunner = GloballyOrderedEventStoreRunner $ \action -> makeDynamicTVarStore >>= atomically . uncurry action

makeDynamicTVarStore :: IO (EventStore CounterEvent STM, GloballyOrderedEventStore CounterEvent STM)
makeDynamicTVarStore = do
  (store, globalStore) <- memoryEventStore
  let
    store' = serializedEventStore dynamicSerializer store
    globalStore' = serializedGloballyOrderedEventStore dynamicSerializer globalStore
  return (store', globalStore')

stateStoreRunner :: EventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreRunner = EventStoreRunner $ \action -> evalStateT (action stateEventStore) emptyEventMap

stateStoreGlobalRunner :: GloballyOrderedEventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreGlobalRunner = GloballyOrderedEventStoreRunner $
  \action -> evalStateT (action stateEventStore stateGloballyOrderedEventStore) emptyEventMap

data EmbeddedState serialized
  = EmbeddedState
  { _embeddedDummyArgument :: Int
  , embeddedEventMap :: EventMap serialized
  }

setEventMap :: EmbeddedState serialized -> EventMap serialized -> EmbeddedState serialized
setEventMap state' eventMap = state' { embeddedEventMap = eventMap }

emptyEmbeddedState :: EmbeddedState serialized
emptyEmbeddedState = EmbeddedState 100 emptyEventMap

embeddedStateStoreRunner :: EventStoreRunner (StateT (EmbeddedState CounterEvent) IO)
embeddedStateStoreRunner = EventStoreRunner $ \action -> evalStateT (action store) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap

embeddedStateStoreGlobalRunner :: GloballyOrderedEventStoreRunner (StateT (EmbeddedState CounterEvent) IO)
embeddedStateStoreGlobalRunner = GloballyOrderedEventStoreRunner $
  \action -> evalStateT (action store globalStore) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    globalStore = embeddedStateGloballyOrderedEventStore embeddedEventMap
