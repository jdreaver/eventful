module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Test.Hspec

import Eventful.Serializer
import Eventful.Store.Memory
import Eventful.TestHelpers

import MemoryTestImport

spec :: Spec
spec = do
  describe "TVar memory event store with actual event type" $ do
    eventStoreSpec tvarRunner
    globalStreamEventStoreSpec tvarGlobalRunner

  describe "TVar memory event store with Dynamic serialized type" $ do
    eventStoreSpec tvarDynamicRunner
    globalStreamEventStoreSpec tvarDynamicGlobalRunner

  describe "MonadState memory event store with actual event type" $ do
    eventStoreSpec stateStoreRunner
    globalStreamEventStoreSpec stateStoreGlobalRunner

  describe "MonadState embedded memory event store with actual event type" $ do
    eventStoreSpec embeddedStateStoreRunner
    globalStreamEventStoreSpec embeddedStateStoreGlobalRunner

tvarRunner :: EventStoreRunner STM
tvarRunner = EventStoreRunner $ \action -> do
  eventTVar <- eventMapTVar
  let
    writer = tvarEventStoreWriter eventTVar
    reader = tvarEventStoreReader eventTVar
  atomically $ action writer reader

tvarGlobalRunner :: GlobalStreamEventStoreRunner STM
tvarGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  eventTVar <- eventMapTVar
  let
    writer = tvarEventStoreWriter eventTVar
    globalReader = tvarGlobalEventStoreReader eventTVar
  atomically $ action writer globalReader

tvarDynamicRunner :: EventStoreRunner STM
tvarDynamicRunner = EventStoreRunner $ \action -> do
  eventTVar <- eventMapTVar
  let
    writer = serializedEventStoreWriter dynamicSerializer $ tvarEventStoreWriter eventTVar
    reader = serializedVersionedEventStoreReader dynamicSerializer $ tvarEventStoreReader eventTVar
  atomically $ action writer reader

tvarDynamicGlobalRunner :: GlobalStreamEventStoreRunner STM
tvarDynamicGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  eventTVar <- eventMapTVar
  let
    writer = serializedEventStoreWriter dynamicSerializer $ tvarEventStoreWriter eventTVar
    globalReader = serializedGlobalEventStoreReader dynamicSerializer $ tvarGlobalEventStoreReader eventTVar
  atomically $ action writer globalReader

stateStoreRunner :: EventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreRunner = EventStoreRunner $ \action -> evalStateT (action stateEventStoreWriter stateEventStoreReader) emptyEventMap

stateStoreGlobalRunner :: GlobalStreamEventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreGlobalRunner = GlobalStreamEventStoreRunner $
  \action -> evalStateT (action stateEventStoreWriter stateGlobalEventStoreReader) emptyEventMap

embeddedStateStoreRunner :: EventStoreRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
embeddedStateStoreRunner = EventStoreRunner $ \action -> evalStateT (action writer reader) emptyEmbeddedState
  where
    writer = embeddedStateEventStoreWriter embeddedEventMap setEventMap
    reader = embeddedStateEventStoreReader embeddedEventMap

embeddedStateStoreGlobalRunner :: GlobalStreamEventStoreRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
embeddedStateStoreGlobalRunner = GlobalStreamEventStoreRunner $
  \action -> evalStateT (action writer globalReader) emptyEmbeddedState
  where
    writer = embeddedStateEventStoreWriter embeddedEventMap setEventMap
    globalReader = embeddedStateGlobalEventStoreReader embeddedEventMap
