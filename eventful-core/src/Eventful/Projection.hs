module Eventful.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  , getLatestProjection
  )
  where

import Data.Foldable (foldl')
import Data.List (scanl')

import Eventful.Store.Class
import Eventful.UUID

-- | A 'Projection' is a piece of @state@ that is constructed only from @event@s.
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
-- produced. Just a 'scanl' using 'projectionEventHandler'.
allProjections :: Projection state event -> [event] -> [state]
allProjections (Projection seed handler) = scanl' handler seed

-- | Gets the latest projection from a store by using 'getEvents' and then
-- applying the events to the event handler.
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
