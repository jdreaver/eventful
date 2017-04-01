module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Test.Hspec

import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store" $ do
    eventStoreSpec makeStore (const atomically)
    sequencedEventStoreSpec makeGlobalStore (const atomically)

makeStore :: IO (MemoryEventStore, ())
makeStore = do
  (store, _, ()) <- makeGlobalStore
  return (store, ())

makeGlobalStore :: IO (MemoryEventStore, GloballyOrderedMemoryEventStore, ())
makeGlobalStore = do
  (store, globalStore) <- memoryEventStore
  return (store, globalStore, ())
