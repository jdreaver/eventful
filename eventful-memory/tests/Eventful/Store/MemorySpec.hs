module Eventful.Store.MemorySpec (spec) where

import Control.Concurrent.STM
import Control.Monad.State.Strict
import Test.Hspec

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
tvarRunner = EventStoreRunner $ \action -> (tvarEventStore <$> eventMapTVar) >>= atomically . action

tvarGlobalRunner :: GlobalStreamEventStoreRunner STM
tvarGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  tvar <- eventMapTVar
  let
    store = tvarEventStore tvar
    globalStore = tvarGlobalStreamEventStore tvar
  atomically $ action store globalStore

tvarDynamicRunner :: EventStoreRunner STM
tvarDynamicRunner = EventStoreRunner $ \action -> (fst <$> makeDynamicTVarStore) >>= atomically . action

tvarDynamicGlobalRunner :: GlobalStreamEventStoreRunner STM
tvarDynamicGlobalRunner = GlobalStreamEventStoreRunner $ \action -> makeDynamicTVarStore >>= atomically . uncurry action

stateStoreRunner :: EventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreRunner = EventStoreRunner $ \action -> evalStateT (action stateEventStore) emptyEventMap

stateStoreGlobalRunner :: GlobalStreamEventStoreRunner (StateT (EventMap CounterEvent) IO)
stateStoreGlobalRunner = GlobalStreamEventStoreRunner $
  \action -> evalStateT (action stateEventStore stateGlobalStreamEventStore) emptyEventMap

embeddedStateStoreRunner :: EventStoreRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
embeddedStateStoreRunner = EventStoreRunner $ \action -> evalStateT (action store) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap

embeddedStateStoreGlobalRunner :: GlobalStreamEventStoreRunner (StateT (StreamEmbeddedState Counter CounterEvent) IO)
embeddedStateStoreGlobalRunner = GlobalStreamEventStoreRunner $
  \action -> evalStateT (action store globalStore) emptyEmbeddedState
  where
    store = embeddedStateEventStore embeddedEventMap setEventMap
    globalStore = embeddedStateGlobalStreamEventStore embeddedEventMap
