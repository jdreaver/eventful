{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.Store.Memory
  ( tvarEventStore
  , tvarGloballyOrderedEventStore
  , stateEventStore
  , stateGloballyOrderedEventStore
  , embeddedStateEventStore
  , embeddedStateGloballyOrderedEventStore
  , EventMap
  , emptyEventMap
  , eventMapTVar
  , module Eventful.Store.Class
  ) where

import Control.Concurrent.STM
import Control.Monad.State.Class
import Data.Foldable (toList)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq

import Eventful.Store.Class
import Eventful.UUID

-- | Internal data structure used for the in-memory event stores.
data EventMap serialized
  = EventMap
  { _eventMapUuidMap :: Map UUID (Seq (GloballyOrderedEvent serialized))
  , _eventMapSeqNum :: SequenceNumber
  -- TODO: Add projection cache here
  }
  deriving (Show)

-- | What it says on the tin, an initialized empty 'EventMap'
emptyEventMap :: EventMap serialized
emptyEventMap = EventMap Map.empty 0

-- | Initialize an 'EventMap' in a 'TVar'
eventMapTVar :: IO (TVar (EventMap serialized))
eventMapTVar = newTVarIO emptyEventMap

-- | An 'EventStore' that stores events in a 'TVar' and runs in 'STM'. This
-- functions initializes the store by creating the 'TVar' and hooking up the
-- event store API to that 'TVar'.
tvarEventStore :: TVar (EventMap serialized) -> EventStore serialized STM
tvarEventStore tvar =
  let
    getLatestVersion uuid = flip latestEventVersion uuid <$> readTVar tvar
    getEvents uuid range = (\s -> lookupEventsInRange s uuid range) <$> readTVar tvar
    storeEvents' uuid events = modifyTVar' tvar (\store -> storeEventMap store uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}

-- | Analog of 'tvarEventStore' for a 'GloballyOrderedEventStore'
tvarGloballyOrderedEventStore :: TVar (EventMap serialized) -> GloballyOrderedEventStore serialized STM
tvarGloballyOrderedEventStore tvar =
  let
    getSequencedEvents range = flip lookupEventMapRange range <$> readTVar tvar
  in GloballyOrderedEventStore{..}

-- | Specialized version of 'embeddedStateEventStore' that only contains an
-- 'EventMap' in the state.
stateEventStore
  :: (MonadState (EventMap serialized) m)
  => EventStore serialized m
stateEventStore = embeddedStateEventStore id (flip const)

-- | An 'EventStore' that runs on some 'MonadState' that contains an
-- 'EventMap'. This is useful if you want to include other state in your
-- 'MonadState'.
embeddedStateEventStore
  :: (MonadState s m)
  => (s -> EventMap serialized)
  -> (s -> EventMap serialized -> s)
  -> EventStore serialized m
embeddedStateEventStore getMap setMap =
  let
    getLatestVersion uuid = flip latestEventVersion uuid <$> gets getMap
    getEvents uuid range = (\s -> lookupEventsInRange s uuid range) <$> gets getMap
    storeEvents' uuid events = modify' (modifyStore uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}
  where
    modifyStore uuid events state' =
      let
        store = getMap state'
        store' = storeEventMap store uuid events
      in setMap state' store'

-- | Analogous to 'stateEventStore' for a 'GloballyOrderedEventStore'.
stateGloballyOrderedEventStore
  :: (MonadState (EventMap serialized) m)
  => GloballyOrderedEventStore serialized m
stateGloballyOrderedEventStore = embeddedStateGloballyOrderedEventStore id

-- | Analogous to 'embeddedStateEventStore' for a 'GloballyOrderedEventStore'.
embeddedStateGloballyOrderedEventStore
  :: (MonadState s m)
  => (s -> EventMap serialized)
  -> GloballyOrderedEventStore serialized m
embeddedStateGloballyOrderedEventStore getMap =
  let
    getSequencedEvents range = flip lookupEventMapRange range <$> gets getMap
  in GloballyOrderedEventStore{..}

lookupEventMapRaw :: EventMap serialized -> UUID -> Seq (StoredEvent serialized)
lookupEventMapRaw (EventMap uuidMap _) uuid =
  fmap globallyOrderedEventToStoredEvent $ fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsInRange :: EventMap serialized -> UUID -> EventStoreQueryRange EventVersion -> [StoredEvent serialized]
lookupEventsInRange store uuid range = filterEventsByRange range' 0 rawEvents
  where
    range' = unEventVersion <$> range
    rawEvents = toList $ lookupEventMapRaw store uuid

filterEventsByRange :: EventStoreQueryRange Int -> Int -> [event] -> [event]
filterEventsByRange EventStoreQueryRange{..} defaultStart events =
  let
    (start', events') =
      case eventStoreQueryRangeStart of
        StartFromBeginning -> (defaultStart, events)
        StartQueryAt start -> (start, drop (start - defaultStart) events)
    events'' =
      case eventStoreQueryRangeLimit of
        NoQueryLimit -> events'
        MaxNumberOfEvents num -> take num events'
        StopQueryAt stop -> take (stop - start' + 1) events'
  in events''

latestEventVersion :: EventMap serialized -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupEventMapRaw store uuid) - 1

lookupEventMapRange :: EventMap serialized -> EventStoreQueryRange SequenceNumber -> [GloballyOrderedEvent serialized]
lookupEventMapRange (EventMap uuidMap _) range = filterEventsByRange range' 1 rawEvents
  where
    range' = unSequenceNumber <$> range
    rawEvents =
      sortOn globallyOrderedEventSequenceNumber $
      concat $
      toList <$> toList uuidMap

storeEventMap
  :: EventMap serialized -> UUID -> [serialized] -> EventMap serialized
storeEventMap store@(EventMap uuidMap seqNum) uuid events =
  let
    versStart = latestEventVersion store uuid + 1
    storedEvents = zipWith storedEventToGloballyOrderedEvent [seqNum + 1..] $ zipWith (StoredEvent uuid) [versStart..] events
    newMap = Map.insertWith (flip (><)) uuid (Seq.fromList storedEvents) uuidMap
    newSeq = seqNum + (SequenceNumber $ length events)
  in EventMap newMap newSeq
