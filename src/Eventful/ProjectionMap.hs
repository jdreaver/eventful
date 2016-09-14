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
newtype ProjectionMap proj
  = ProjectionMap { unProjectionMap :: Map UUID proj }
  deriving (Show)

projectionMap :: ProjectionMap a
projectionMap = ProjectionMap Map.empty

applyProjectionMap :: (Projection a) => UUID -> Event a -> ProjectionMap a -> ProjectionMap a
applyProjectionMap uuid event (ProjectionMap map') =
  ProjectionMap (Map.insert uuid newAP map')
  where
    newAP = case Map.lookup uuid map' of
      Nothing -> apply seed event
      Just ap -> apply ap event

lookupProjectionMap :: (Projection proj) => UUID -> ProjectionMap proj -> proj
lookupProjectionMap uuid (ProjectionMap map') = fromMaybe seed (Map.lookup uuid map')
