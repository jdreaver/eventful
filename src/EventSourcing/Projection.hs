{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines an Aggregate type-class from DDD parlance.

module EventSourcing.Projection
  ( Projection (..)
  , latestProjection
  , allProjections
  )
  where

import Data.Foldable (foldl')
import Data.List (scanl')

-- | A state object is 'Projection' when it is created as the result of
-- applying events.
class Projection s where
  type Event s :: *

  seed :: s
  apply :: s -> Event s -> s

-- | Computes the latest state of a Projection from some Events.
latestProjection :: (Foldable t, Projection a) => t (Event a) -> a
latestProjection = foldl' apply seed

-- | Given a list of events, produce all the Projections that were ever
-- produced.
allProjections :: (Projection a) => [Event a] -> [a]
allProjections = scanl' apply seed
