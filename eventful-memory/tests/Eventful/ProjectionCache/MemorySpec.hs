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
    writer = tvarEventStoreWriter eventTVar
    reader = tvarEventStoreReader eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action writer reader cache

tvarGlobalStreamProjectionCacheRunner :: GlobalStreamProjectionCacheRunner STM
tvarGlobalStreamProjectionCacheRunner = GlobalStreamProjectionCacheRunner $ \action -> do
  eventTVar <- eventMapTVar
  projTVar <- projectionMapTVar
  let
    writer = tvarEventStoreWriter eventTVar
    globalReader = tvarGlobalEventStoreReader eventTVar
    cache = tvarProjectionCache projTVar
  atomically $ action writer globalReader cache

stateVersionedProjectionCacheRunner :: VersionedProjectionCacheRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
stateVersionedProjectionCacheRunner = VersionedProjectionCacheRunner $ \action -> evalStateT (action writer reader cache) emptyEmbeddedState
  where
    writer = embeddedStateEventStoreWriter embeddedEventMap setEventMap
    reader = embeddedStateEventStoreReader embeddedEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap

stateGlobalStreamProjectionCacheRunner :: GlobalStreamProjectionCacheRunner (StateT (GlobalStreamEmbeddedState Counter CounterEvent Text) IO)
stateGlobalStreamProjectionCacheRunner =
  GlobalStreamProjectionCacheRunner $ \action -> evalStateT (action writer globalReader cache) emptyEmbeddedState
  where
    writer = embeddedStateEventStoreWriter embeddedEventMap setEventMap
    globalReader = embeddedStateGlobalEventStoreReader embeddedEventMap
    cache = embeddedStateProjectionCache embeddedProjectionMap setProjectionMap
