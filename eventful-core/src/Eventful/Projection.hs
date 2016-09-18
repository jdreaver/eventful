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
class Projection proj where
  data Event proj :: *

  seed :: proj
  apply :: proj -> Event proj -> proj

-- | Computes the latest state of a Projection from some Events.
latestProjection :: (Foldable t, Projection a) => t (Event a) -> a
latestProjection = foldl' apply seed

-- | Given a list of events, produce all the Projections that were ever
-- produced. Just a 'scanl' using 'apply'.
allProjections :: (Projection a) => [Event a] -> [a]
allProjections = scanl' apply seed
