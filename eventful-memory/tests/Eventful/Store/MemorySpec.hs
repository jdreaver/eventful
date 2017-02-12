module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Test.Hspec

import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "TVar memory event store" $ do
    eventStoreSpec makeStore (const atomically)
    sequencedEventStoreSpec memoryGetGloballyOrderedEvents makeStore (const atomically)

makeStore :: IO (MemoryEventStore, ())
makeStore = (,()) <$> memoryEventStore
