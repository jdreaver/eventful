module Eventful.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  )
  where

import Data.Foldable (foldl')
import Data.List (scanl')

-- | A 'Projection' is a piece of state that is constructed only from applying
-- events. For those coming from a Data Driven Design background, a Projection
-- is the current state of an 'Aggregate'.
data Projection state event
  = Projection
  { projectionSeed :: state
    -- ^ Initial state of a projection
  , projectionApply :: state -> event -> state
    -- ^ The function that applies and event to the current state, producing a
    -- new state.
  }

-- | Computes the latest state of a Projection from some Events.
latestProjection :: (Foldable t) => Projection state event -> t event -> state
latestProjection (Projection seed apply) = foldl' apply seed

-- | Given a list of events, produce all the Projections that were ever
-- produced. Just a 'scanl' using 'apply'.
allProjections :: Projection state event -> [event] -> [state]
allProjections (Projection seed apply) = scanl' apply seed
