module Eventful.EventBus.PipesSpec (spec) where

import Test.Hspec

import Eventful.EventBus.Pipes
import Eventful.Store.Memory
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "IORef memory event store and Sync event bus" $ do
    eventBusSpec pipesEventBus memoryEventStoreIORef (Just $ EventBusSpecDelayMilliseconds 50)
