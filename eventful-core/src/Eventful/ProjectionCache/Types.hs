{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.ProjectionCache.Types
  ( ProjectionCache (..)
  , VersionedProjectionCache
  , GlobalStreamProjectionCache
  , runProjectionCacheUsing
  , serializedProjectionCache
  , getLatestVersionedProjectionWithCache
  , getLatestGlobalProjectionWithCache
  , updateProjectionCache
  , updateGlobalProjectionCache
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
-- the stream 'UUID' and 'EventVersion'. It is recommended to use the other
-- helper functions in this module to interpret the stored values using a
-- 'Projection'.
--
-- The @key@ and @position@ type parameters are polymorphic so we can abstract
-- over a cache for individual event streams, and a cache for globally ordered
-- streams.
data ProjectionCache key position serialized m
  = ProjectionCache
  { storeProjectionSnapshot :: key -> position -> serialized -> m ()
    -- ^ Stores the state for a projection at a given @key@ and @position@.
    -- This is pretty unsafe, because there is no guarantee what is stored is
    -- actually derived from the events in the stream. Consider using
    -- 'updateProjectionCache'.
  , loadProjectionSnapshot :: key -> m (Maybe (position, serialized))
    -- ^ Loads the latest projection state from the cache.
  }

-- | Type synonym for a 'ProjectionCache' used on individual event streams.
type VersionedProjectionCache serialized m = ProjectionCache UUID EventVersion serialized m

-- | Type synonym for a 'ProjectionCache' that is used in conjunction with a
-- 'GlobalStreamEventStore'.
type GlobalStreamProjectionCache key serialized m = ProjectionCache key SequenceNumber serialized m

-- | Changes the monad a 'ProjectionCache' runs in. This is useful to run the
-- cache in another 'Monad' while forgetting the original 'Monad'.
runProjectionCacheUsing
  :: (Monad m, Monad mstore)
  => (forall a. mstore a -> m a)
  -> ProjectionCache key position serialized mstore
  -> ProjectionCache key position serialized m
runProjectionCacheUsing runCache ProjectionCache{..} =
  ProjectionCache
  { storeProjectionSnapshot = \uuid version state -> runCache $ storeProjectionSnapshot uuid version state
  , loadProjectionSnapshot = runCache . loadProjectionSnapshot
  }

-- | Wraps a 'ProjectionCache' and transparently serializes/deserializes events for
-- you. Note that in this implementation deserialization errors when using
-- 'getEvents' are simply ignored (the event is not returned).
serializedProjectionCache
  :: (Monad m)
  => Serializer state serialized
  -> ProjectionCache key position serialized m
  -> ProjectionCache key position state m
serializedProjectionCache Serializer{..} ProjectionCache{..} =
  ProjectionCache storeProjectionSnapshot' loadProjectionSnapshot'
  where
    storeProjectionSnapshot' uuid version = storeProjectionSnapshot uuid version . serialize
    loadProjectionSnapshot' uuid = do
      mState <- loadProjectionSnapshot uuid
      return $ mState >>= traverse deserialize

-- | Like 'getLatestVersionedProjection', but uses a 'ProjectionCache' if it contains
-- more recent state.
getLatestVersionedProjectionWithCache
  :: (Monad m)
  => VersionedEventStoreReader m event
  -> VersionedProjectionCache state m
  -> VersionedStreamProjection state event
  -> m (VersionedStreamProjection state event)
getLatestVersionedProjectionWithCache store cache projection =
  getLatestProjectionWithCache' cache projection (streamProjectionKey projection) >>= getLatestStreamProjection store

-- | Like 'getLatestGlobalProjection', but uses a 'ProjectionCache' if it
-- contains more recent state.
getLatestGlobalProjectionWithCache
  :: (Monad m)
  => GlobalEventStoreReader m event
  -> GlobalStreamProjectionCache key state m
  -> GlobalStreamProjection state event
  -> key
  -> m (GlobalStreamProjection state event)
getLatestGlobalProjectionWithCache store cache projection key =
  getLatestProjectionWithCache' cache projection key >>= getLatestStreamProjection store

getLatestProjectionWithCache'
  :: (Monad m, Ord position)
  => ProjectionCache key position state m
  -> StreamProjection projKey position state event
  -> key
  -> m (StreamProjection projKey position state event)
getLatestProjectionWithCache' cache projection key = do
  mLatestState <- loadProjectionSnapshot cache key
  let
    mkProjection' (position, state) =
      if position > streamProjectionPosition projection
      then
        projection
        { streamProjectionPosition = position
        , streamProjectionState = state
        }
      else projection
  return $ maybe projection mkProjection' mLatestState

-- | Loads the latest projection state from the cache/store and stores this
-- value back into the projection cache.
updateProjectionCache
  :: (Monad m)
  => VersionedEventStoreReader m event
  -> VersionedProjectionCache state m
  -> VersionedStreamProjection state event
  -> m ()
updateProjectionCache reader cache projection = do
  StreamProjection{..} <- getLatestVersionedProjectionWithCache reader cache projection
  storeProjectionSnapshot cache streamProjectionKey streamProjectionPosition streamProjectionState

-- | Analog of 'updateProjectionCache' for a 'GlobalStreamProjectionCache'.
updateGlobalProjectionCache
  :: (Monad m)
  => GlobalEventStoreReader m event
  -> GlobalStreamProjectionCache key state m
  -> GlobalStreamProjection state event
  -> key
  -> m ()
updateGlobalProjectionCache reader cache projection key = do
  StreamProjection{..} <- getLatestGlobalProjectionWithCache reader cache projection key
  storeProjectionSnapshot cache key streamProjectionPosition streamProjectionState
