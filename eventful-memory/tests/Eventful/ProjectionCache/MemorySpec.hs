module Eventful.ProjectionCache.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Test.Hspec

import Eventful.ProjectionCache.Memory
import Eventful.Store.Memory
import Eventful.TestHelpers

import MemoryTestImport

spec :: Spec
spec = do
  describe "TVar projection cache" $ do
    projectionCacheSpec tvarProjectionCacheRunner

  describe "MonadState embedded memory projection cache" $ do
    projectionCacheSpec stateProjectionCacheRunner

tvarProjectionCacheRunner :: ProjectionCacheRunner STM
tvarProjectionCacheRunner = ProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let
    store = tvarEventStore eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action store cache

stateProjectionCacheRunner :: ProjectionCacheRunner (StateT (EmbeddedState Counter CounterEvent) IO)
stateProjectionCacheRunner = ProjectionCacheRunner $ \action -> evalStateT (action store cache) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap
