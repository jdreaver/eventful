module EventSourcing.Store.MemorySnapshot
  ( MemorySnapshotStore
  , memorySnapshotStore
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import EventSourcing.Projection
import EventSourcing.Store.Class
import EventSourcing.UUID

-- | Wraps a given event store and stores the latest projections in memory.
data MemorySnapshotStore m store serialized proj
  = MemorySnapshotStore
  { _memorySnapshotStoreEventStore :: SerializedEventStore m store serialized (Event proj) => store
  -- TODO: Make the value type (EventVersion, ByteString) and use this for
  -- latestEventVersion.
  , _memorySnapshotStoreProjections :: TVar (Map UUID proj)
  }

memorySnapshotStore :: (MonadIO m) => store -> m (MemorySnapshotStore m store serialized proj)
memorySnapshotStore store = do
  tvar <- liftIO . atomically $ newTVar Map.empty
  return $ MemorySnapshotStore store tvar

getSnapshotProjection
  :: (MonadIO m, Projection proj, SerializedEventStore m store serialized (Event proj))
  => MemorySnapshotStore m store serialized proj -> UUID -> m proj
getSnapshotProjection (MemorySnapshotStore store tvar) uuid = do
  proj <- liftIO . atomically $ Map.lookup uuid <$> readTVar tvar
  case proj of
    -- Cache hit, we're good
    (Just p) -> return p
    -- Cache miss, check the store
    Nothing -> do
      proj' <- getAggregateFromSerialized store (AggregateId uuid)
      liftIO . atomically $ modifyTVar' tvar (Map.insert uuid proj')
      return proj'

instance
  (Projection proj, Event proj ~ event, MonadIO m, SerializedEventStore m store serialized event)
  => SerializedEventStore m (MemorySnapshotStore m store serialized proj) serialized event where
  getSerializedEvents (MemorySnapshotStore store _) = getSerializedEvents store
  getAllSerializedEvents (MemorySnapshotStore store _) = getAllSerializedEvents store
  storeSerializedEvents mstore@(MemorySnapshotStore store tvar) uuid events = do
    proj <- getSnapshotProjection mstore uuid
    storedEvents <- storeSerializedEvents store uuid events
    liftIO . atomically $ do
      let proj' = foldl' apply proj events
      modifyTVar' tvar (Map.insert uuid proj')
    return storedEvents

instance
  (MonadIO m, Projection proj, SerializedEventStore m store serialized (Event proj))
  => CachedEventStore m (MemorySnapshotStore m store serialized proj) serialized proj where
  getAggregate store (AggregateId uuid) = getSnapshotProjection store uuid
