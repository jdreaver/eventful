module Eventful.Store.MemorySpec (spec) where

import Test.Hspec

import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store" $ do
    eventStoreSpec memoryEventStoreTVar
    sequencedEventStoreSpec memoryEventStoreTVar

  describe "IORef memory event store" $ do
    eventStoreSpec memoryEventStoreIORef
    sequencedEventStoreSpec memoryEventStoreIORef
