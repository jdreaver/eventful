module MemoryTestImport
  ( makeDynamicTVarStore
  , EmbeddedState (..)
  , StreamEmbeddedState
  , GloballyOrderedEmbeddedState
  , emptyEmbeddedState
  , setEventMap
  , setProjectionMap
  ) where

import Control.Concurrent.STM

import Eventful.ProjectionCache.Memory
import Eventful.Serializer
import Eventful.Store.Memory
import Eventful.TestHelpers
import Eventful.UUID

makeDynamicTVarStore :: IO (EventStore CounterEvent STM, GloballyOrderedEventStore CounterEvent STM)
makeDynamicTVarStore = do
  tvar <- eventMapTVar
  let
    store = tvarEventStore tvar
    globalStore = tvarGloballyOrderedEventStore tvar
    store' = serializedEventStore dynamicSerializer store
    globalStore' = serializedGloballyOrderedEventStore dynamicSerializer globalStore
  return (store', globalStore')

data EmbeddedState state event key orderKey
  = EmbeddedState
  { _embeddedDummyArgument :: Int
  , embeddedEventMap :: EventMap event
  , embeddedProjectionMap :: ProjectionMap key orderKey state
  }

type StreamEmbeddedState state event = EmbeddedState state event UUID EventVersion
type GloballyOrderedEmbeddedState state event key = EmbeddedState state event key SequenceNumber

emptyEmbeddedState :: EmbeddedState state event key orderKey
emptyEmbeddedState = EmbeddedState 100 emptyEventMap emptyProjectionMap

setEventMap :: EmbeddedState state event key orderKey -> EventMap event -> EmbeddedState state event key orderKey
setEventMap state' eventMap = state' { embeddedEventMap = eventMap }

setProjectionMap
  :: EmbeddedState state event key orderKey
  -> ProjectionMap key orderKey state
  -> EmbeddedState state event key orderKey
setProjectionMap state' projectionMap = state' { embeddedProjectionMap = projectionMap }
