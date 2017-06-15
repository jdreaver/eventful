module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Test.Hspec

import Eventful.Serializer
import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store with Dynamic serialized type" $ do
    eventStoreSpec makeTVarDynamicStore (const atomically)
    sequencedEventStoreSpec makeTVarDynamicGlobalStore (const atomically)

  describe "TVar memory event store with actual event type" $ do
    eventStoreSpec makeTVarStore (const atomically)
    sequencedEventStoreSpec makeTVarGlobalStore (const atomically)

  describe "MonadState memory event store with actual event type" $ do
    eventStoreSpec makeStateStore (const (flip evalStateT emptyEventMap))
    sequencedEventStoreSpec makeStateGlobalStore (const (flip evalStateT emptyEventMap))

makeTVarStore :: IO (EventStore serialized STM, ())
makeTVarStore = do
  (store, _, ()) <- makeTVarGlobalStore
  return (store, ())

makeTVarGlobalStore :: IO (EventStore serialized STM, GloballyOrderedEventStore serialized STM, ())
makeTVarGlobalStore = do
  (store, globalStore) <- memoryEventStore
  return (store, globalStore, ())

makeTVarDynamicStore :: IO (EventStore CounterEvent STM, ())
makeTVarDynamicStore = do
  (store, _, ()) <- makeTVarDynamicGlobalStore
  return (store, ())

makeTVarDynamicGlobalStore :: IO (EventStore CounterEvent STM, GloballyOrderedEventStore CounterEvent STM, ())
makeTVarDynamicGlobalStore = do
  (store, globalStore) <- memoryEventStore
  let
    store' = serializedEventStore dynamicSerializer store
    globalStore' = serializedGloballyOrderedEventStore dynamicSerializer globalStore
  return (store', globalStore', ())

makeStateStore :: IO (EventStore serialized (StateT (EventMap serialized) IO), ())
makeStateStore = return (stateEventStore, ())

makeStateGlobalStore
  :: IO (EventStore serialized (StateT (EventMap serialized) IO), GloballyOrderedEventStore serialized (StateT (EventMap serialized) IO), ())
makeStateGlobalStore = return (stateEventStore, stateGloballyOrderedEventStore, ())
