module Eventful.Store.Memory
  ( MemoryEventStore
  , GloballyOrderedMemoryEventStore
  , memoryEventStore
  , memoryEventStoreGetAllUuids
  , module Eventful.Store.Class
  ) where

import Control.Concurrent.STM
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

data EventMap
  = EventMap
  { _eventMapUuidMap :: Map UUID (Seq (GloballyOrderedEvent (StoredEvent Dynamic)))
  , _eventMapSeqNum :: SequenceNumber
  -- TODO: Add projection cache here
  }
  deriving (Show)

type MemoryEventStore = EventStore Dynamic STM
type GloballyOrderedMemoryEventStore = GloballyOrderedEventStore Dynamic STM

-- | Initializes memory event stores.
memoryEventStore :: IO (MemoryEventStore, GloballyOrderedMemoryEventStore)
memoryEventStore = do
  tvar <- newTVarIO (EventMap Map.empty 0)
  let
    getLatestVersion uuid = flip latestEventVersion uuid <$> readTVar tvar
    getEvents uuid vers = toList . (\s -> lookupEventsFromVersion s uuid vers) <$> readTVar tvar
    storeEvents' uuid events = modifyTVar' tvar (\store -> storeEventMap store uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
    getSequencedEvents seqNum = flip lookupEventMapSeq seqNum <$> readTVar tvar
  return (EventStore{..}, GloballyOrderedEventStore{..})

memoryEventStoreGetAllUuids :: TVar EventMap -> STM [UUID]
memoryEventStoreGetAllUuids tvar = fmap fst . Map.toList . _eventMapUuidMap <$> readTVar tvar

lookupEventMapRaw :: EventMap -> UUID -> Seq (StoredEvent Dynamic)
lookupEventMapRaw (EventMap uuidMap _) uuid =
   fmap globallyOrderedEventEvent $ fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsFromVersion :: EventMap -> UUID -> Maybe EventVersion -> Seq (StoredEvent Dynamic)
lookupEventsFromVersion store uuid Nothing = lookupEventMapRaw store uuid
lookupEventsFromVersion store uuid (Just (EventVersion vers)) = Seq.drop vers $ lookupEventMapRaw store uuid

latestEventVersion :: EventMap -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupEventMapRaw store uuid) - 1

lookupEventMapSeq :: EventMap -> SequenceNumber -> [GloballyOrderedEvent (StoredEvent Dynamic)]
lookupEventMapSeq (EventMap uuidMap _) seqNum =
  sortOn globallyOrderedEventSequenceNumber $
  filter ((> seqNum) . globallyOrderedEventSequenceNumber) $
  concat $
  toList <$> toList uuidMap

storeEventMap
  :: EventMap -> UUID -> [Dynamic] -> EventMap
storeEventMap store@(EventMap uuidMap seqNum) uuid events =
  let
    versStart = latestEventVersion store uuid + 1
    storedEvents = zipWith GloballyOrderedEvent [seqNum + 1..] $ zipWith (StoredEvent uuid) [versStart..] events
    newMap = Map.insertWith (flip (><)) uuid (Seq.fromList storedEvents) uuidMap
    newSeq = seqNum + (SequenceNumber $ length events)
  in EventMap newMap newSeq
