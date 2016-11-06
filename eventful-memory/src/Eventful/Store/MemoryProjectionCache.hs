module Eventful.Store.MemoryProjectionCache
  ( ProjectionCache (..)
  , projectionCache
  , CachedProjection (..)
  , serializeCached
  , deserializeCached
  , loadProjectionCached
  , ProjectionCacheStore (..)
  , projectionCacheStore
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
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

serializeCached :: (Serializable proj Dynamic) => CachedProjection proj -> CachedProjection Dynamic
serializeCached (CachedProjection vers proj) = CachedProjection vers (serialize proj)

deserializeCached :: (Serializable proj Dynamic) => CachedProjection Dynamic -> Maybe (CachedProjection proj)
deserializeCached (CachedProjection vers proj) = CachedProjection vers <$> deserialize proj

lookupProjectionCache
  :: (Projection proj, Serializable proj Dynamic)
  => ProjectionId proj -> ProjectionCache -> CachedProjection proj
lookupProjectionCache (ProjectionId uuid) (ProjectionCache cache) =
  case Map.lookup uuid cache >>= deserializeCached of
    (Just proj) -> proj
    Nothing -> CachedProjection (-1) seed

-- | Load a 'Projection' and refresh the given 'ProjectionCache' for that
-- projection.
loadProjectionCached
  :: (EventStore m store serialized, Projection proj, Serializable proj Dynamic, Serializable (Event proj) serialized)
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

-- | Wraps a given event store with an in-memory projection cache.
data ProjectionCacheStore store
  = ProjectionCacheStore
  { _projectionCacheStoreCache :: TVar ProjectionCache
  , _projectionCacheStoreSubStore :: store
  }

projectionCacheStore :: (MonadIO m) => store -> m (ProjectionCacheStore store)
projectionCacheStore store = do
  tvar <- liftIO $ newTVarIO projectionCache
  return $ ProjectionCacheStore tvar store

-- TODO: This is constrained to Dynamic because Projection serialization and
-- event serialization are the same thing right now. We would need a separate
-- class to make caching projections more efficient.
instance (MonadIO m, EventStore m store Dynamic) => EventStore m (ProjectionCacheStore store) Dynamic where
  getAllUuids (ProjectionCacheStore _ store) = getAllUuids store
  getEventsRaw (ProjectionCacheStore _ store) = getEventsRaw store
  getEventsFromVersionRaw (ProjectionCacheStore _ store) = getEventsFromVersionRaw store
  getLatestVersion (ProjectionCacheStore _ store) = getLatestVersion store
  getSequencedEvents (ProjectionCacheStore _ store) = getSequencedEvents store
  storeEventsRaw (ProjectionCacheStore _ store) = storeEventsRaw store
  getLatestProjection (ProjectionCacheStore tvar store) pid = do
    cache <- liftIO $ readTVarIO tvar
    (cache', proj) <- loadProjectionCached store pid cache
    liftIO . atomically $ writeTVar tvar cache'
    return proj
