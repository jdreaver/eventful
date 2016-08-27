module EventSourcing.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  , eventProjectionName
  )
  where

import Data.Foldable (foldl')
import Data.List (scanl')
import Data.Typeable

-- | A state object is 'Projection' when it is created as the result of
-- applying events.
class (Typeable proj) => Projection proj where
  data Event proj :: *

  projectionName :: proj -> String
  projectionName = show . typeOf

  seed :: proj
  apply :: proj -> Event proj -> proj

-- | Computes the latest state of a Projection from some Events.
latestProjection :: (Foldable t, Projection a) => t (Event a) -> a
latestProjection = foldl' apply seed

-- | Given a list of events, produce all the Projections that were ever
-- produced.
allProjections :: (Projection a) => [Event a] -> [a]
allProjections = scanl' apply seed


eventProjectionName :: (Projection proj) => Event proj -> String
eventProjectionName = projectionName . mkProj
  where mkProj :: (Projection proj) => Event proj -> proj
        mkProj _ = undefined
