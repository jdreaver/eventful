module Eventful.Store.MemoryProjectionCache
  ( ProjectionCache (..)
  , projectionCache
  , CachedProjection (..)
  , serializeCached
  , deserializeCached
  , loadProjectionCached
  ) where

import Data.Dynamic
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Eventful.Projection
import Eventful.Serializable
import Eventful.Store.Class
import Eventful.UUID

-- | An in-memory cache of projections. This cache works in conjunction with
-- 'loadProjectionCached' and is refreshed for a particular projection whenever
-- that projection is loaded.
newtype ProjectionCache = ProjectionCache { _unProjectionCache :: Map UUID (CachedProjection Dynamic) }
  deriving (Show)

projectionCache :: ProjectionCache
projectionCache = ProjectionCache Map.empty

data CachedProjection proj
  = CachedProjection
  { _cachedProjectionVersion :: EventVersion
  , _cachedProjectionProjection :: proj
  }
  deriving (Show, Eq)

serializeCached :: (Typeable proj) => CachedProjection proj -> CachedProjection Dynamic
serializeCached (CachedProjection vers proj) = CachedProjection vers (serialize proj)

deserializeCached :: (Typeable proj) => CachedProjection Dynamic -> Maybe (CachedProjection proj)
deserializeCached (CachedProjection vers proj) = CachedProjection vers <$> deserialize proj

lookupProjectionCache
  :: (Projection proj, Typeable proj)
  => ProjectionId proj -> ProjectionCache -> CachedProjection proj
lookupProjectionCache (ProjectionId uuid) (ProjectionCache cache) =
  case Map.lookup uuid cache >>= deserializeCached of
    (Just proj) -> proj
    Nothing -> CachedProjection (-1) seed

-- | Load a 'Projection' and refresh the given 'ProjectionCache' for that
-- projection.
loadProjectionCached
  :: (EventStore m store serialized, Projection proj, Typeable proj, Serializable (Event proj) serialized)
  => store -> ProjectionId proj -> ProjectionCache -> m (ProjectionCache, proj)
loadProjectionCached store pid@(ProjectionId uuid) cache@(ProjectionCache projMap) = do
  version <- getLatestVersion store uuid
  let
    (CachedProjection cachedVersion cachedProj) = lookupProjectionCache pid cache
  if cachedVersion >= version
  then return (cache, cachedProj)
  else do
    newEvents <- getEventsFromVersion store pid (cachedVersion + 1)
    let
      proj = foldl' apply cachedProj (storedEventEvent <$> newEvents)
      cachedProj' = CachedProjection version proj
      cache' = ProjectionCache $ Map.insert uuid (serializeCached cachedProj') projMap
    return (cache', proj)
