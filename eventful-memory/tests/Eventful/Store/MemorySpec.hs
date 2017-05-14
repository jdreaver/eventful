module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Test.Hspec

import Eventful.Serializer
import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store with Dynamic serialized type" $ do
    eventStoreSpec makeDynamicStore (const atomically)
    sequencedEventStoreSpec makeDynamicGlobalStore (const atomically)

  describe "TVar memory event store with actual event type" $ do
    eventStoreSpec makeStore (const atomically)
    sequencedEventStoreSpec makeGlobalStore (const atomically)

makeStore :: IO (EventStore serialized STM, ())
makeStore = do
  (store, _, ()) <- makeGlobalStore
  return (store, ())

makeGlobalStore :: IO (EventStore serialized STM, GloballyOrderedEventStore serialized STM, ())
makeGlobalStore = do
  (store, globalStore) <- memoryEventStore
  return (store, globalStore, ())

makeDynamicStore :: IO (EventStore CounterEvent STM, ())
makeDynamicStore = do
  (store, _, ()) <- makeDynamicGlobalStore
  return (store, ())

makeDynamicGlobalStore :: IO (EventStore CounterEvent STM, GloballyOrderedEventStore CounterEvent STM, ())
makeDynamicGlobalStore = do
  (store, globalStore) <- memoryEventStore
  let
    store' = serializedEventStore dynamicSerializer store
    globalStore' = serializedGloballyOrderedEventStore dynamicSerializer globalStore
  return (store', globalStore', ())
