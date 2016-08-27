module EventSourcing.Store.MemorySpec (spec) where

import TestImport

import Test.Hspec

import EventSourcing.Store.Memory

spec :: Spec
spec = do
  describe "TVar memory event store" $ do
    eventStoreSpec memoryEventStoreTVar
    sequencedEventStoreSpec memoryEventStoreTVar

  describe "IORef memory event store" $ do
    eventStoreSpec memoryEventStoreIORef
    sequencedEventStoreSpec memoryEventStoreIORef
