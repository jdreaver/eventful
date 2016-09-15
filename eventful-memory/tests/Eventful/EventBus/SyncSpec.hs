module Eventful.EventBus.SyncSpec (spec) where

import Test.Hspec

import Eventful.EventBus.Sync
import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "IORef memory event store and Sync event bus" $ do
    eventBusSpec (syncEventBusIO :: IO (SyncEventBusIO IO serialized)) memoryEventStoreIORef Nothing
