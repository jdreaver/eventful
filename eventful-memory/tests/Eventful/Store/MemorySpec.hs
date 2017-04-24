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

makeStore :: IO (MemoryEventStore serialized, ())
makeStore = do
  (store, _, ()) <- makeGlobalStore
  return (store, ())

makeGlobalStore :: IO (MemoryEventStore serialized, GloballyOrderedMemoryEventStore serialized, ())
makeGlobalStore = do
  (store, globalStore) <- memoryEventStore
  return (store, globalStore, ())

makeDynamicStore :: IO (MemoryEventStore CounterEvent, ())
makeDynamicStore = do
  (store, _, ()) <- makeDynamicGlobalStore
  return (store, ())

makeDynamicGlobalStore :: IO (MemoryEventStore CounterEvent, GloballyOrderedMemoryEventStore CounterEvent, ())
makeDynamicGlobalStore = do
  (store, globalStore) <- memoryEventStore
  let
    store' = serializedEventStore dynamicSerializer store
    globalStore' = serializedGloballyOrderedEventStore dynamicSerializer globalStore
  return (store', globalStore', ())
