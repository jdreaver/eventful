module Eventful.ProjectionCache.MemorySpec (spec) where

import Control.Monad.State.Strict
import Test.Hspec

import Eventful.ProjectionCache.Memory
import Eventful.Store.Memory
import Eventful.TestHelpers

import MemoryTestImport

spec :: Spec
spec = do
  describe "MonadState embedded memory projection cache" $ do
    projectionCacheSpec stateProjectionCacheRunner

stateProjectionCacheRunner :: ProjectionCacheRunner (StateT (EmbeddedState Counter CounterEvent) IO)
stateProjectionCacheRunner = ProjectionCacheRunner $ \action -> evalStateT (action store cache) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap
