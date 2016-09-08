module EventSourcing.Store.Memory
  ( MemoryEventStore (..)
  , memoryEventStoreTVar
  , memoryEventStoreIORef
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable (toList)
import Data.IORef
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Safe (maximumDef)

import EventSourcing.Store.Class
import TodoCommon

data MemoryEventStore
  = MemoryEventStore { unMemoryEventStore :: Seq (StoredEvent Dynamic)}
  deriving (Show)

memoryEventStoreTVar :: IO (TVar MemoryEventStore)
memoryEventStoreTVar = newTVarIO (MemoryEventStore Seq.empty)

memoryEventStoreIORef :: IO (IORef MemoryEventStore)
memoryEventStoreIORef = newIORef (MemoryEventStore Seq.empty)

lookupMemoryEventStoreRaw :: MemoryEventStore -> UUID -> [StoredEvent Dynamic]
lookupMemoryEventStoreRaw (MemoryEventStore seq') uuid =
  filter ((==) uuid . storedEventAggregateId) $ toList seq'

latestEventVersion' :: MemoryEventStore -> UUID -> EventVersion
latestEventVersion' store uuid = maximumDef (-1) $ storedEventVersion <$> lookupMemoryEventStoreRaw store uuid

lookupMemoryEventStoreSeq :: MemoryEventStore -> SequenceNumber -> [StoredEvent Dynamic]
lookupMemoryEventStoreSeq (MemoryEventStore seq') (SequenceNumber i) = toList $ Seq.drop i seq'

storeMemoryEventStore
  :: MemoryEventStore -> UUID -> [Dynamic] -> (MemoryEventStore, [StoredEvent Dynamic])
storeMemoryEventStore store@(MemoryEventStore seq') uuid events =
  let versStart = latestEventVersion' store uuid + 1
      seqStart = SequenceNumber (Seq.length seq') + 1
      storedEvents = zipWith3 (StoredEvent uuid) [versStart..] [seqStart..] events
      newSeq = seq' >< Seq.fromList storedEvents
  in (MemoryEventStore newSeq, storedEvents)

instance (MonadIO m) => EventStore m (TVar MemoryEventStore) Dynamic where
  getAllUuids tvar = liftIO $ toList . Set.fromList . map storedEventAggregateId . toList . unMemoryEventStore <$> readTVarIO tvar
  getEventsRaw tvar uuid = liftIO $ flip lookupMemoryEventStoreRaw uuid <$> readTVarIO tvar
  storeEventsRaw tvar uuid events = liftIO . atomically $ do
    store <- readTVar tvar
    let (newMap, storedEvents) = storeMemoryEventStore store uuid events
    writeTVar tvar newMap
    return storedEvents
  latestEventVersion tvar uuid = liftIO $ flip latestEventVersion' uuid <$> readTVarIO tvar

  getSequencedEvents tvar seqNum = liftIO $ do
    store <- readTVarIO tvar
    return $ lookupMemoryEventStoreSeq store seqNum

instance (MonadIO m) => EventStore m (IORef MemoryEventStore) Dynamic where
  getAllUuids ref = liftIO $ toList . Set.fromList . map storedEventAggregateId . toList . unMemoryEventStore <$> readIORef ref
  getEventsRaw ref uuid = liftIO $ flip lookupMemoryEventStoreRaw uuid <$> readIORef ref
  storeEventsRaw ref uuid events = liftIO $ do
    store <- readIORef ref
    let (newMap, storedEvents) = storeMemoryEventStore store uuid events
    writeIORef ref newMap
    return storedEvents
  latestEventVersion ref uuid = liftIO $ flip latestEventVersion' uuid <$> readIORef ref
  getSequencedEvents ref seqNum = liftIO $ do
    store <- readIORef ref
    return $ lookupMemoryEventStoreSeq store seqNum
