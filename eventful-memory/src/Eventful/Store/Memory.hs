module Eventful.Store.Memory
  ( MemoryEventStore
  , MemoryEventStoreM
  , memoryEventStore
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

newtype MemoryEventStore = MemoryEventStore (TVar EventMap)
type MemoryEventStoreM = ReaderT MemoryEventStore STM

data EventMap
  = EventMap
  { _eventMapUuidMap :: Map UUID (Seq (StoredEvent Dynamic))
  , _eventMapSeqNum :: SequenceNumber
  -- TODO: Add projection cache here
  }
  deriving (Show)

memoryEventStore :: IO MemoryEventStore
memoryEventStore = MemoryEventStore <$> newTVarIO (EventMap Map.empty 0)

lookupEventMapRaw :: EventMap -> UUID -> Seq (StoredEvent Dynamic)
lookupEventMapRaw (EventMap uuidMap _) uuid =
  fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsFromVersion :: EventMap -> UUID -> EventVersion -> Seq (StoredEvent Dynamic)
lookupEventsFromVersion store uuid (EventVersion vers) = Seq.drop vers $ lookupEventMapRaw store uuid

latestEventVersion :: EventMap -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupEventMapRaw store uuid) - 1

lookupEventMapSeq :: EventMap -> SequenceNumber -> [StoredEvent Dynamic]
lookupEventMapSeq (EventMap uuidMap _) seqNum =
  sortOn storedEventSequenceNumber $ filter ((> seqNum) . storedEventSequenceNumber) $ concat $ toList <$> toList uuidMap

storeEventMap
  :: EventMap -> UUID -> [Dynamic] -> (EventMap, [StoredEvent Dynamic])
storeEventMap store@(EventMap uuidMap seqNum) uuid events =
  let versStart = latestEventVersion store uuid + 1
      storedEvents = zipWith3 (StoredEvent uuid) [versStart..] [seqNum + 1..] events
      newMap = Map.insertWith (flip (><)) uuid (Seq.fromList storedEvents) uuidMap
      newSeq = seqNum + (SequenceNumber $ length events)
  in (EventMap newMap newSeq, storedEvents)

runMemoryEventStore :: (MonadIO m) => MemoryEventStore -> MemoryEventStoreM a -> m a
runMemoryEventStore store action = liftIO . atomically $ runReaderT action store

instance EventStoreMetadata MemoryEventStoreM where
  getAllUuids = do
    (MemoryEventStore tvar) <- ask
    lift $ fmap fst . Map.toList . _eventMapUuidMap <$> readTVar tvar
  getLatestVersion uuid = do
    (MemoryEventStore tvar) <- ask
    lift $ flip latestEventVersion uuid <$> readTVar tvar

instance EventStore MemoryEventStoreM Dynamic where
  getEventsRaw uuid = do
    (MemoryEventStore tvar) <- ask
    lift $ toList . flip lookupEventMapRaw uuid <$> readTVar tvar
  getEventsFromVersionRaw uuid vers = do
    (MemoryEventStore tvar) <- ask
    lift $ toList . (\s -> lookupEventsFromVersion s uuid vers) <$> readTVar tvar
  storeEventsRaw uuid events = do
    (MemoryEventStore tvar) <- ask
    store <- lift $ readTVar tvar
    let (newMap, storedEvents) = storeEventMap store uuid events
    lift $ writeTVar tvar newMap
    return storedEvents
  getSequencedEvents seqNum = do
    (MemoryEventStore tvar) <- ask
    store <- lift $ readTVar tvar
    return $ lookupEventMapSeq store seqNum
