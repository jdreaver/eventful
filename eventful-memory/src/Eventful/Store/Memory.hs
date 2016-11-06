module Eventful.Store.Memory
  ( MemoryEventStore
  , memoryEventStoreTVar
  , memoryEventStoreIORef
  , module Eventful.Store.Class
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Safe (maximumDef)

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

memoryEventStoreIORef :: IO (IORef MemoryEventStore)
memoryEventStoreIORef = newIORef (MemoryEventStore Map.empty 0)

lookupMemoryEventStoreRaw :: MemoryEventStore -> UUID -> [StoredEvent Dynamic]
lookupMemoryEventStoreRaw (MemoryEventStore uuidMap _) uuid =
  maybe [] toList $ Map.lookup uuid uuidMap

latestEventVersion :: MemoryEventStore -> UUID -> EventVersion
latestEventVersion store uuid = maximumDef (-1) $ storedEventVersion <$> lookupMemoryEventStoreRaw store uuid

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

instance (MonadIO m) => EventStore m (TVar MemoryEventStore) Dynamic where
  getAllUuids tvar = liftIO $ fmap fst . Map.toList . memoryEventStoreUuidMap <$> readTVarIO tvar
  getEventsRaw tvar uuid = liftIO $ flip lookupMemoryEventStoreRaw uuid <$> readTVarIO tvar
  storeEventsRaw tvar uuid events = liftIO . atomically $ do
    store <- readTVar tvar
    let (newMap, storedEvents) = storeMemoryEventStore store uuid events
    writeTVar tvar newMap
    return storedEvents

  getSequencedEvents tvar seqNum = liftIO $ do
    store <- readTVarIO tvar
    return $ lookupMemoryEventStoreSeq store seqNum

instance (MonadIO m) => EventStore m (IORef MemoryEventStore) Dynamic where
  getAllUuids ref = liftIO $ fmap fst . Map.toList . memoryEventStoreUuidMap <$> readIORef ref
  getEventsRaw ref uuid = liftIO $ flip lookupMemoryEventStoreRaw uuid <$> readIORef ref
  storeEventsRaw ref uuid events = liftIO $ do
    store <- readIORef ref
    let (newMap, storedEvents) = storeMemoryEventStore store uuid events
    writeIORef ref newMap
    return storedEvents
  getSequencedEvents ref seqNum = liftIO $ do
    store <- readIORef ref
    return $ lookupMemoryEventStoreSeq store seqNum
