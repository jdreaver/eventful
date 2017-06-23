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
data ProjectionCache serialized m
  = ProjectionCache
  { storeProjection :: serialized -> EventVersion -> UUID -> m ()
    -- ^ Stores the state for a projection at a given 'EventVersion'. This is
    -- pretty unsafe, because there is no guarantee what is stored actually
    -- corresponds to the events in the stream. Consider using
    -- 'updateProjectionCache'.
  , loadCachedProjection :: UUID -> m (Maybe (EventVersion, serialized))
    -- ^ Loads latest projection state from the cache.
  , clearOldProjections :: UUID -> m ()
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
  { storeProjection = \state version uuid -> runCache $ storeProjection state version uuid
  , loadCachedProjection = runCache . loadCachedProjection
  , clearOldProjections = runCache . clearOldProjections
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
  ProjectionCache storeProjection' loadCachedProjection' clearOldProjections
  where
    storeProjection' state = storeProjection (serialize state)
    loadCachedProjection' uuid = do
      mState <- loadCachedProjection uuid
      return $ mState >>= traverse deserialize

-- | Like 'getLatestProjection', but uses a 'ProjectionCache' if it contains
-- more recent state.
getLatestProjectionWithCache
  :: (Monad m)
  => EventStore event m
  -> ProjectionCache state m
  -> StreamProjection state event
  -> m (StreamProjection state event)
getLatestProjectionWithCache store cache projection@StreamProjection{..} = do
  mLatestState <- loadCachedProjection cache streamProjectionUuid
  let
    mkProjection' (version, state) =
      if version > streamProjectionVersion
      then projection { streamProjectionVersion = version, streamProjectionState = state }
      else projection
    projection' = maybe projection mkProjection' mLatestState
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
  storeProjection cache streamProjectionState streamProjectionVersion streamProjectionUuid
