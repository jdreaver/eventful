module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Test.Hspec

import Eventful.Store.Memory
import Eventful.TestHelpers

import MemoryTestImport

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
tvarRunner = EventStoreRunner $ \action -> (tvarEventStore <$> eventMapTVar) >>= atomically . action

tvarGlobalRunner :: GloballyOrderedEventStoreRunner STM
tvarGlobalRunner = GloballyOrderedEventStoreRunner $ \action -> do
  tvar <- eventMapTVar
  let
    store = tvarEventStore tvar
    globalStore = tvarGloballyOrderedEventStore tvar
  atomically $ action store globalStore

tvarDynamicRunner :: EventStoreRunner STM
tvarDynamicRunner = EventStoreRunner $ \action -> (fst <$> makeDynamicTVarStore) >>= atomically . action

tvarDynamicGlobalRunner :: GloballyOrderedEventStoreRunner STM
tvarDynamicGlobalRunner = GloballyOrderedEventStoreRunner $ \action -> makeDynamicTVarStore >>= atomically . uncurry action

stateStoreRunner :: EventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreRunner = EventStoreRunner $ \action -> evalStateT (action stateEventStore) emptyEventMap

stateStoreGlobalRunner :: GloballyOrderedEventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreGlobalRunner = GloballyOrderedEventStoreRunner $
  \action -> evalStateT (action stateEventStore stateGloballyOrderedEventStore) emptyEventMap

embeddedStateStoreRunner :: EventStoreRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
embeddedStateStoreRunner = EventStoreRunner $ \action -> evalStateT (action store) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap

embeddedStateStoreGlobalRunner :: GloballyOrderedEventStoreRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
embeddedStateStoreGlobalRunner = GloballyOrderedEventStoreRunner $
  \action -> evalStateT (action store globalStore) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    globalStore = embeddedStateGloballyOrderedEventStore embeddedEventMap
