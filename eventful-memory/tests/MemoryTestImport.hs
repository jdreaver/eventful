module MemoryTestImport
  ( makeDynamicTVarStore
  , EmbeddedState (..)
  , emptyEmbeddedState
  , setEventMap
  , setProjectionMap
  ) where

import Control.Concurrent.STM

import Eventful.ProjectionCache.Memory
import Eventful.Serializer
import Eventful.Store.Memory
import Eventful.TestHelpers

makeDynamicTVarStore :: IO (EventStore CounterEvent STM, GloballyOrderedEventStore CounterEvent STM)
makeDynamicTVarStore = do
  (store, globalStore) <- memoryEventStore
  let
    store' = serializedEventStore dynamicSerializer store
    globalStore' = serializedGloballyOrderedEventStore dynamicSerializer globalStore
  return (store', globalStore')

data EmbeddedState state event
  = EmbeddedState
  { _embeddedDummyArgument :: Int
  , embeddedEventMap :: EventMap event
  , embeddedProjectionMap :: ProjectionMap state
  }

emptyEmbeddedState :: EmbeddedState state event
emptyEmbeddedState = EmbeddedState 100 emptyEventMap emptyProjectionMap

setEventMap :: EmbeddedState state event -> EventMap event -> EmbeddedState state event
setEventMap state' eventMap = state' { embeddedEventMap = eventMap }

setProjectionMap :: EmbeddedState state event -> ProjectionMap state -> EmbeddedState state event
setProjectionMap state' projectionMap = state' { embeddedProjectionMap = projectionMap }
