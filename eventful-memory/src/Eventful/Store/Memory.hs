module Eventful.Store.Memory
  ( MemoryEventStore
  , memoryEventStoreTVar
  , runMemoryEventStore
  , module Eventful.Store.Class
  ) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Dynamic
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

import Eventful.Store.Class
import Eventful.UUID

data MemoryEventStore
  = MemoryEventStore
  { memoryEventStoreUuidMap :: Map UUID (Seq (StoredEvent Dynamic))
  , _memoryEventStoreSeqNum :: SequenceNumber
  -- TODO: Add projection cache here
  }
  deriving (Show)

memoryEventStoreTVar :: IO (TVar MemoryEventStore)
memoryEventStoreTVar = newTVarIO (MemoryEventStore Map.empty 0)

lookupMemoryEventStoreRaw :: MemoryEventStore -> UUID -> Seq (StoredEvent Dynamic)
lookupMemoryEventStoreRaw (MemoryEventStore uuidMap _) uuid =
  fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsFromVersion :: MemoryEventStore -> UUID -> EventVersion -> Seq (StoredEvent Dynamic)
lookupEventsFromVersion store uuid (EventVersion vers) = Seq.drop vers $ lookupMemoryEventStoreRaw store uuid

latestEventVersion :: MemoryEventStore -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupMemoryEventStoreRaw store uuid) - 1

lookupMemoryEventStoreSeq :: MemoryEventStore -> SequenceNumber -> [StoredEvent Dynamic]
lookupMemoryEventStoreSeq (MemoryEventStore uuidMap _) seqNum =
  sortOn storedEventSequenceNumber $ filter ((> seqNum) . storedEventSequenceNumber) $ concat $ toList <$> toList uuidMap

storeMemoryEventStore
  :: MemoryEventStore -> UUID -> [Dynamic] -> (MemoryEventStore, [StoredEvent Dynamic])
storeMemoryEventStore store@(MemoryEventStore uuidMap seqNum) uuid events =
  let versStart = latestEventVersion store uuid + 1
      storedEvents = zipWith3 (StoredEvent uuid) [versStart..] [seqNum + 1..] events
      newMap = Map.insertWith (flip (><)) uuid (Seq.fromList storedEvents) uuidMap
      newSeq = seqNum + (SequenceNumber $ length events)
  in (MemoryEventStore newMap newSeq, storedEvents)

runMemoryEventStore :: (MonadIO m) => TVar MemoryEventStore -> ReaderT (TVar MemoryEventStore) STM a -> m a
runMemoryEventStore tvar action = liftIO . atomically $ runReaderT action tvar

instance EventStoreMetadata (ReaderT (TVar MemoryEventStore) STM) where
  getAllUuids = do
    tvar <- ask
    lift $ fmap fst . Map.toList . memoryEventStoreUuidMap <$> readTVar tvar
  getLatestVersion uuid = do
    tvar <- ask
    lift $ flip latestEventVersion uuid <$> readTVar tvar

instance EventStore (ReaderT (TVar MemoryEventStore) STM) Dynamic where
  getEventsRaw uuid = do
    tvar <- ask
    lift $ toList . flip lookupMemoryEventStoreRaw uuid <$> readTVar tvar
  getEventsFromVersionRaw uuid vers = do
    tvar <- ask
    lift $ toList . (\s -> lookupEventsFromVersion s uuid vers) <$> readTVar tvar
  storeEventsRaw uuid events = do
    tvar <- ask
    store <- lift $ readTVar tvar
    let (newMap, storedEvents) = storeMemoryEventStore store uuid events
    lift $ writeTVar tvar newMap
    return storedEvents
  getSequencedEvents seqNum = do
    tvar <- ask
    store <- lift $ readTVar tvar
    return $ lookupMemoryEventStoreSeq store seqNum
