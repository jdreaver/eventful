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
data Projection proj event
  = Projection
  { projectionSeed :: proj
  , projectionApply :: proj -> event -> proj
  }

-- | Computes the latest state of a Projection from some Events.
latestProjection :: (Foldable t) => Projection proj event -> t event -> proj
latestProjection (Projection seed apply) = foldl' apply seed

-- | Given a list of events, produce all the Projections that were ever
-- produced. Just a 'scanl' using 'apply'.
allProjections :: Projection proj event -> [event] -> [proj]
allProjections (Projection seed apply) = scanl' apply seed
