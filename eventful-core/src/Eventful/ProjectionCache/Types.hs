{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.ProjectionCache.Types
  ( ProjectionCache (..)
  , runProjectionCacheUsing
  , serializedProjectionCache
  , getLatestProjectionWithCache
  , updateProjectionCache
  ) where

import Eventful.Projection
import Eventful.Serializer
import Eventful.Store.Class
import Eventful.UUID

-- | A 'ProjectionCache' caches snapshots of 'Projection's in event streams.
-- This is useful if your event streams are very large. This cache operates on
-- some 'Monad' @m@ and stores the 'Projection' state of type @serialized@.
--
-- At its core, this is essentially just a key-value store with knowledge of
-- the stream 'UUID' and 'EventVersion'. The key is to use the other helper
-- functions in this module to interpret the stored values using a
-- 'Projection'.
data ProjectionCache serialized m
  = ProjectionCache
  { storeProjectionSnapshot :: UUID -> EventVersion -> serialized -> m ()
    -- ^ Stores the state for a projection at a given 'EventVersion'. This is
    -- pretty unsafe, because there is no guarantee what is stored actually
    -- corresponds to the events in the stream. Consider using
    -- 'updateProjectionCache'.
  , loadProjectionSnapshot :: UUID -> m (Maybe (EventVersion, serialized))
    -- ^ Loads latest projection state from the cache.
  , clearSnapshots :: UUID -> m ()
    -- ^ Clears all projections in the cache that are not the latest
    -- projections.
  }

-- | Changes the monad an 'ProjectionCache' runs in. This is useful to run the
-- cache in another 'Monad' while forgetting the original 'Monad'.
runProjectionCacheUsing
  :: (Monad m, Monad mstore)
  => (forall a. mstore a -> m a)
  -> ProjectionCache serialized mstore
  -> ProjectionCache serialized m
runProjectionCacheUsing runCache ProjectionCache{..} =
  ProjectionCache
  { storeProjectionSnapshot = \uuid version state -> runCache $ storeProjectionSnapshot uuid version state
  , loadProjectionSnapshot = runCache . loadProjectionSnapshot
  , clearSnapshots = runCache . clearSnapshots
  }

-- | Wraps a 'ProjectionCache' and transparently serializes/deserializes events for
-- you. Note that in this implementation deserialization errors when using
-- 'getEvents' are simply ignored (the event is not returned).
serializedProjectionCache
  :: (Monad m)
  => Serializer state serialized
  -> ProjectionCache serialized m
  -> ProjectionCache state m
serializedProjectionCache Serializer{..} ProjectionCache{..} =
  ProjectionCache storeProjectionSnapshot' loadProjectionSnapshot' clearSnapshots
  where
    storeProjectionSnapshot' uuid version = storeProjectionSnapshot uuid version . serialize
    loadProjectionSnapshot' uuid = do
      mState <- loadProjectionSnapshot uuid
      return $ mState >>= traverse deserialize

-- | Like 'getLatestProjection', but uses a 'ProjectionCache' if it contains
-- more recent state.
getLatestProjectionWithCache
  :: (Monad m)
  => EventStore event m
  -> ProjectionCache state m
  -> StreamProjection state event
  -> m (StreamProjection state event)
getLatestProjectionWithCache store cache originalProj = do
  mLatestState <- loadProjectionSnapshot cache (streamProjectionUuid originalProj)
  let
    mkProjection' (version, state) =
      if version > streamProjectionVersion originalProj
      then originalProj { streamProjectionVersion = version, streamProjectionState = state }
      else originalProj
    projection' = maybe originalProj mkProjection' mLatestState
  getLatestProjection store projection'

-- | Loads the latest projection state from the cache/store and stores this
-- value back into the projection cache.
updateProjectionCache
  :: (Monad m)
  => EventStore event m
  -> ProjectionCache state m
  -> StreamProjection state event
  -> m ()
updateProjectionCache store cache projection = do
  StreamProjection{..} <- getLatestProjectionWithCache store cache projection
  storeProjectionSnapshot cache streamProjectionUuid streamProjectionVersion streamProjectionState
