module Eventful.ProjectionMap
  ( ProjectionMap (..)
  , projectionMap
  , applyProjectionMap
  , lookupProjectionMap
  ) where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Eventful.Projection
import Eventful.UUID

-- | Holds multiple 'Projection's in memory. Useful for in-memory event stores.
newtype ProjectionMap state
  = ProjectionMap { unProjectionMap :: Map UUID state }
  deriving (Show)

-- | Creates a new, empty 'ProjectionMap'.
projectionMap :: ProjectionMap a
projectionMap = ProjectionMap Map.empty

-- | Variant of 'apply' just for 'ProjectionMap's.
applyProjectionMap :: Projection state event -> UUID -> event -> ProjectionMap state -> ProjectionMap state
applyProjectionMap (Projection seed apply) uuid event (ProjectionMap map') =
  ProjectionMap (Map.insert uuid newAP map')
  where
    newAP = case Map.lookup uuid map' of
      Nothing -> apply seed event
      Just ap -> apply ap event

-- | Get the current state of a 'Projection' from the 'ProjectionMap'.
lookupProjectionMap :: Projection state event -> UUID -> ProjectionMap state -> state
lookupProjectionMap (Projection seed _) uuid (ProjectionMap map') = fromMaybe seed (Map.lookup uuid map')
