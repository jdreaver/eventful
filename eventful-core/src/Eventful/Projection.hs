{-# LANGUAGE RecordWildCards #-}

module Eventful.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  , StreamProjection (..)
  , VersionedStreamProjection
  , GlobalStreamProjection
  , streamProjection
  , versionedStreamProjection
  , globalStreamProjection
  , streamProjectionEventHandler
  , getLatestStreamProjection
  , serializedProjection
  , projectionMapMaybe
  ) where

import Data.Foldable (foldl')
import Data.Functor.Contravariant
import Data.List (scanl')

import Eventful.Serializer
import Eventful.Store.Class
import Eventful.UUID

-- | A 'Projection' is a piece of @state@ that is constructed only from
-- @event@s. A Projection is how you reconstruct event sourced state from the
-- ordered stream of events that constitute that state. The "seed" of a
-- Projection is the initial state before any events are applied. The event
-- handler for a projection is the function that actually modifies state based
-- on the given event.
data Projection state event
  = Projection
  { projectionSeed :: state
    -- ^ Initial state of a projection
  , projectionEventHandler :: state -> event -> state
    -- ^ The function that applies and event to the current state, producing a
    -- new state.
  }

instance Contravariant (Projection state) where
  contramap f (Projection seed handler) = Projection seed handler'
    where
      handler' state event = handler state (f event)

-- | Computes the latest state of a 'Projection' from some events.
latestProjection :: (Foldable t) => Projection state event -> t event -> state
latestProjection (Projection seed handler) = foldl' handler seed

-- | Given a list of events, produce all the Projections that were ever
-- produced. Just a 'scanl' using 'projectionEventHandler'. This function is
-- useful for testing 'Projection's; you can easily assert that all the states
-- of a Projection are valid given a list of events.
allProjections :: Projection state event -> [event] -> [state]
allProjections (Projection seed handler) = scanl' handler seed

-- | A 'StreamProjection' is a 'Projection' that has been constructed from
-- events from a particular event stream. This is useful when we want to cache
-- the resulting state and also keep track of what part of the stream the state
-- is caught up to.
data StreamProjection key position state event
  = StreamProjection
  { streamProjectionKey :: !key
  , streamProjectionPosition :: !position
  , streamProjectionProjection :: !(Projection state event)
  , streamProjectionState :: !state
  }

type VersionedStreamProjection = StreamProjection UUID EventVersion
type GlobalStreamProjection state event = StreamProjection () SequenceNumber state (VersionedStreamEvent event)

-- | Initialize a 'StreamProjection' with a 'Projection', key, and order key.
streamProjection
  :: key
  -> position
  -> Projection state event
  -> StreamProjection key position state event
streamProjection key position projection@Projection{..} =
  StreamProjection key position projection projectionSeed

-- | Initialize a 'VersionedStreamProjection'.
versionedStreamProjection
  :: UUID
  -> Projection state event
  -> VersionedStreamProjection state event
versionedStreamProjection uuid = streamProjection uuid (-1)

-- | Initialize a 'GlobalStreamProjection'.
globalStreamProjection
  :: Projection state (VersionedStreamEvent event)
  -> GlobalStreamProjection state event
globalStreamProjection = streamProjection () 0

-- | Apply an event to the 'StreamProjection'. NOTE: There is no guarantee that
-- the order key for the event is greater than the current order key in the
-- 'StreamProjection'. This function will simply update the 'StreamProjection'
-- to use the order key of the event.
streamProjectionEventHandler
  :: StreamProjection key position state event
  -> StreamEvent eventKey position event
  -> StreamProjection key position state event
streamProjectionEventHandler StreamProjection{..} event =
  let
    Projection{..} = streamProjectionProjection
    position' = streamEventPosition event
    state' = projectionEventHandler streamProjectionState (streamEventEvent event)
  in StreamProjection streamProjectionKey position' streamProjectionProjection state'

-- | Gets the latest projection from a store by querying events from the latest
-- order key and then applying the events using the Projection's event handler.
getLatestStreamProjection
  :: (Monad m, Num position)
  => EventStoreReader key position m (StreamEvent key position event)
  -> StreamProjection key position state event
  -> m (StreamProjection key position state event)
getLatestStreamProjection (EventStoreReader getEvents') projection@StreamProjection{..} = do
  events <- getEvents' (eventsStartingAt streamProjectionKey $ streamProjectionPosition + 1)
  return $ foldl' streamProjectionEventHandler projection events

-- | Use a 'Serializer' to wrap a 'Projection' with event type @event@ so it
-- uses the @serialized@ type.
serializedProjection
  :: Projection state event
  -> Serializer event serialized
  -> Projection state serialized
serializedProjection proj Serializer{..} = projectionMapMaybe deserialize proj

-- | Transform a 'Projection' when you only have a partial relationship between
-- the source event type and the target event type.
projectionMapMaybe
  :: (eventB -> Maybe eventA)
  -> Projection state eventA
  -> Projection state eventB
projectionMapMaybe f (Projection seed handler) = Projection seed handler'
  where
    handler' state = maybe state (handler state) . f
