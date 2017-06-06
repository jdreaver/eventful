{-# LANGUAGE RecordWildCards #-}

module Eventful.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  , getLatestProjection
  , getLatestGlobalProjection
  , serializedProjection
  )
  where

import Data.Foldable (foldl')
import Data.List (scanl')
import Data.Maybe (fromMaybe)

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
  events <- getEvents store uuid Nothing
  let
    latestVersion = maxEventVersion events
    latestProj = latestProjection proj $ storedEventEvent <$> events
  return (latestProj, latestVersion)
  where
    maxEventVersion [] = -1
    maxEventVersion es = maximum $ storedEventVersion <$> es

-- | Gets globally ordered events from the event store and builds a
-- 'Projection' based on 'ProjectionEvent'. Optionally accepts the current
-- projection state as an argument.
getLatestGlobalProjection
  :: (Monad m)
  => GloballyOrderedEventStore serialized m
  -> Projection proj (ProjectionEvent serialized)
  -> Maybe (proj, SequenceNumber)
  -> m (proj, SequenceNumber)
getLatestGlobalProjection store proj mCurrentState = do
  let
    currentState = fromMaybe (projectionSeed proj) $ fst <$> mCurrentState
    startingSequenceNumber = maybe 0 (+1) $ snd <$> mCurrentState
  events <- getSequencedEvents store startingSequenceNumber
  let
    projectionEvents = globallyOrderedEventToProjectionEvent <$> events
    latestState = foldl' (projectionEventHandler proj) currentState projectionEvents
    latestSeq =
      case events of
        [] -> startingSequenceNumber
        _ -> globallyOrderedEventSequenceNumber $ last events
  return (latestState, latestSeq)

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
