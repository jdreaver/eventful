module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Test.Hspec

import Eventful
import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store with Dynamic serialized type" $ do
    eventStoreSpec makeStore (const atomically) dynamicSerializer
    sequencedEventStoreSpec makeGlobalStore (const atomically) dynamicSerializer

  describe "TVar memory event store with actual event type" $ do
    let serializer = idSerializer :: Serializer CounterEvent CounterEvent
    eventStoreSpec makeStore (const atomically) serializer
    sequencedEventStoreSpec makeGlobalStore (const atomically) serializer

makeStore :: IO (MemoryEventStore serialized, ())
makeStore = do
  (store, _, ()) <- makeGlobalStore
  return (store, ())

makeGlobalStore :: IO (MemoryEventStore serialized, GloballyOrderedMemoryEventStore serialized, ())
makeGlobalStore = do
  (store, globalStore) <- memoryEventStore
  return (store, globalStore, ())
