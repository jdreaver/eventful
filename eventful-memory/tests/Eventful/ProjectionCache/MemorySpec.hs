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
    streamProjectionCacheSpec tvarStreamProjectionCacheRunner
    globallyOrderedProjectionCacheSpec tvarGloballyOrderedProjectionCacheRunner

  describe "MonadState embedded memory projection cache" $ do
    streamProjectionCacheSpec stateStreamProjectionCacheRunner
    globallyOrderedProjectionCacheSpec stateGloballyOrderedProjectionCacheRunner

tvarStreamProjectionCacheRunner :: StreamProjectionCacheRunner STM
tvarStreamProjectionCacheRunner = StreamProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let
    store = tvarEventStore eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action store cache

tvarGloballyOrderedProjectionCacheRunner :: GloballyOrderedProjectionCacheRunner STM
tvarGloballyOrderedProjectionCacheRunner = GloballyOrderedProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let
    store = tvarEventStore eventTVar
    globalStore = tvarGloballyOrderedEventStore eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action store globalStore cache

stateStreamProjectionCacheRunner :: StreamProjectionCacheRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
stateStreamProjectionCacheRunner = StreamProjectionCacheRunner $ \action -> evalStateT (action store cache) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap

stateGloballyOrderedProjectionCacheRunner :: GloballyOrderedProjectionCacheRunner (StateT (GloballyOrderedEmbeddedState Counter CounterEvent Text) IO)
stateGloballyOrderedProjectionCacheRunner =
  GloballyOrderedProjectionCacheRunner $ \action -> evalStateT (action store globalStore cache) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    globalStore = embeddedStateGloballyOrderedEventStore embeddedEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap
