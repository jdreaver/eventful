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

-- | Wraps a given event store and stores the latest projections in memory.
data MemorySnapshotStore m store proj
  = MemorySnapshotStore
  { _memorySnapshotStoreEventStore :: EventStore m store proj => store
  -- TODO: Make the value type (EventVersion, ByteString) and use this for
  -- latestEventVersion.
  , _memorySnapshotStoreProjections :: TVar (Map (AggregateId proj) proj)
  }

memorySnapshotStore :: (MonadIO m) => store -> m (MemorySnapshotStore m store proj)
memorySnapshotStore store = do
  tvar <- liftIO . atomically $ newTVar Map.empty
  return $ MemorySnapshotStore store tvar

getSnapshotProjection
  :: (MonadIO m, Projection proj, EventStore m store proj)
  => MemorySnapshotStore m store proj -> AggregateId proj -> m proj
getSnapshotProjection (MemorySnapshotStore store tvar) uuid = do
  proj <- liftIO . atomically $ Map.lookup uuid <$> readTVar tvar
  case proj of
    -- Cache hit, we're good
    (Just p) -> return p
    -- Cache miss, check the store
    Nothing -> do
      proj' <- getAggregate store uuid
      liftIO . atomically $ modifyTVar' tvar (Map.insert uuid proj')
      return proj'

instance
  (Projection proj, Event proj ~ event, MonadIO m, EventStore m store proj)
  => EventStore m (MemorySnapshotStore m store proj) proj where
  getEvents (MemorySnapshotStore store _) = getEvents store
  storeEvents mstore@(MemorySnapshotStore store tvar) uuid events = do
    proj <- getSnapshotProjection mstore uuid
    storedEvents <- storeEvents store uuid events
    liftIO . atomically $ do
      let proj' = foldl' apply proj events
      modifyTVar' tvar (Map.insert uuid proj')
    return storedEvents
  latestEventVersion (MemorySnapshotStore store _) = latestEventVersion store

instance
  ( MonadIO m
  , EventStore m store proj
  , SequencedEventStore m store event
  )
  => SequencedEventStore m (MemorySnapshotStore m store proj) event where
  getSequencedEvents (MemorySnapshotStore store _) = getSequencedEvents store
