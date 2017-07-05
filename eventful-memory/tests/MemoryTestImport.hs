module MemoryTestImport
  ( EmbeddedState (..)
  , StreamEmbeddedState
  , GlobalStreamEmbeddedState
  , emptyEmbeddedState
  , setEventMap
  , setProjectionMap
  ) where

import Eventful.ProjectionCache.Memory
import Eventful.Store.Memory
import Eventful.UUID

data EmbeddedState state event key position
  = EmbeddedState
  { _embeddedDummyArgument :: Int
  , embeddedEventMap :: EventMap event
  , embeddedProjectionMap :: ProjectionMap key position state
  }

type StreamEmbeddedState state event = EmbeddedState state event UUID EventVersion
type GlobalStreamEmbeddedState state event key = EmbeddedState state event key SequenceNumber

emptyEmbeddedState :: EmbeddedState state event key position
emptyEmbeddedState = EmbeddedState 100 emptyEventMap emptyProjectionMap

setEventMap :: EmbeddedState state event key position -> EventMap event -> EmbeddedState state event key position
setEventMap state' eventMap = state' { embeddedEventMap = eventMap }

setProjectionMap
  :: EmbeddedState state event key position
  -> ProjectionMap key position state
  -> EmbeddedState state event key position
setProjectionMap state' projectionMap = state' { embeddedProjectionMap = projectionMap }
