{-# LANGUAGE RecordWildCards #-}

module Eventful.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  , getLatestProjection
  , GloballyOrderedProjection (..)
  , initGloballyOrderedProjection
  , globallyOrderedProjectionEventHandler
  , getLatestGlobalProjection
  , serializedProjection
  )
  where

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

-- | Gets the latest projection from a store by using 'getEvents' and then
-- applying the events using the Projection's event handler.
getLatestProjection
  :: (Monad m)
  => EventStore serialized m
  -> Projection proj serialized
  -> UUID
  -> m (proj, EventVersion)
getLatestProjection store proj uuid = do
  events <- getEvents store uuid allEvents
  let
    latestVersion = maxEventVersion events
    latestProj = latestProjection proj $ storedEventEvent <$> events
  return (latestProj, latestVersion)
  where
    maxEventVersion [] = -1
    maxEventVersion es = maximum $ storedEventVersion <$> es

-- | This is a combination of a 'Projection' and the latest projection state
-- with respect to some 'SequenceNumber'. This is useful for in-memory read
-- models, and for querying the latest state starting from some previous state
-- at a lower 'SequenceNumber'.
data GloballyOrderedProjection proj serialized
  = GloballyOrderedProjection
  { globallyOrderedProjectionProjection :: !(Projection proj (GloballyOrderedEvent serialized))
  , globallyOrderedProjectionSequenceNumber :: !SequenceNumber
  , globallyOrderedProjectionState :: !proj
  }

-- | Initialize a 'GloballyOrderedProjection' at 'SequenceNumber' 0 and with
-- the projection's seed value.
initGloballyOrderedProjection
  :: Projection proj (GloballyOrderedEvent serialized)
  -> GloballyOrderedProjection proj serialized
initGloballyOrderedProjection projection@Projection{..} =
  GloballyOrderedProjection projection 0 projectionSeed

-- | This applies an event to a 'GloballyOrderedProjection'. NOTE: There is no
-- guarantee that the 'SequenceNumber' for the event is the previous
-- 'SequenceNumber' plus one (in fact, that isn't even a guarantee that some
-- stores can provide). This function will update the
-- 'GloballyOrderedProjetion' to use the sequence number of the event.
globallyOrderedProjectionEventHandler
  :: GloballyOrderedProjection proj serialized
  -> GloballyOrderedEvent serialized
  -> GloballyOrderedProjection proj serialized
globallyOrderedProjectionEventHandler GloballyOrderedProjection{..} event@GloballyOrderedEvent{..} =
  let
    Projection{..} = globallyOrderedProjectionProjection
    seqNum = globallyOrderedEventSequenceNumber
    state' = projectionEventHandler globallyOrderedProjectionState event
  in GloballyOrderedProjection globallyOrderedProjectionProjection seqNum state'

-- | Gets globally ordered events from the event store and builds a
-- 'Projection' based on 'ProjectionEvent'. Optionally accepts the current
-- projection state as an argument.
getLatestGlobalProjection
  :: (Monad m)
  => GloballyOrderedEventStore serialized m
  -> GloballyOrderedProjection proj serialized
  -> m (GloballyOrderedProjection proj serialized)
getLatestGlobalProjection store globalProjection@GloballyOrderedProjection{..} = do
  events <- getSequencedEvents store (eventsStartingAt $ globallyOrderedProjectionSequenceNumber + 1)
  return $ foldl' globallyOrderedProjectionEventHandler globalProjection events

-- | Use a 'Serializer' to wrap a 'Projection' with event type @event@ so it
-- uses the @serialized@ type.
serializedProjection
  :: Projection state event
  -> Serializer event serialized
  -> Projection state serialized
serializedProjection (Projection seed eventHandler) Serializer{..} =
  Projection seed serializedHandler
  where
    -- Try to deserialize the event and apply the handler. If we can't
    -- deserialize, then just return the state.
    serializedHandler state = maybe state (eventHandler state) . deserialize
