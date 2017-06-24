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
    versionedProjectionCacheSpec tvarVersionedProjectionCacheRunner
    globalStreamProjectionCacheSpec tvarGlobalStreamProjectionCacheRunner

  describe "MonadState embedded memory projection cache" $ do
    versionedProjectionCacheSpec stateVersionedProjectionCacheRunner
    globalStreamProjectionCacheSpec stateGlobalStreamProjectionCacheRunner

tvarVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner STM
tvarVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let
    store = tvarEventStore eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action store cache

tvarGlobalStreamProjectionCacheRunner :: GlobalStreamProjectionCacheRunner STM
tvarGlobalStreamProjectionCacheRunner = GlobalStreamProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let
    store = tvarEventStore eventTVar
    globalStore = tvarGlobalStreamEventStore eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action store globalStore cache

stateVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
stateVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> evalStateT (action store cache) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap

stateGlobalStreamProjectionCacheRunner :: GlobalStreamProjectionCacheRunner (StateT (GlobalStreamEmbeddedState Counter CounterEvent Text) IO)
stateGlobalStreamProjectionCacheRunner =
  GlobalStreamProjectionCacheRunner $ \action -> evalStateT (action store globalStore cache) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    globalStore = embeddedStateGlobalStreamEventStore embeddedEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap
