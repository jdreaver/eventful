{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.Store.Memory
  ( tvarEventStore
  , tvarGlobalStreamEventStore
  , stateEventStore
  , stateGlobalStreamEventStore
  , embeddedStateEventStore
  , embeddedStateGlobalStreamEventStore
  , EventMap
  , emptyEventMap
  , eventMapTVar
  , module Eventful.Store.Class
  ) where

import Control.Concurrent.STM
import Control.Monad.State.Class
import Data.Foldable (toList)
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
  { _eventMapUuidMap :: Map UUID (Seq (VersionedStreamEvent serialized))
  , _eventMapGlobalEvents :: Seq (VersionedStreamEvent serialized)
  }
  deriving (Show)

-- | What it says on the tin, an initialized empty 'EventMap'
emptyEventMap :: EventMap serialized
emptyEventMap = EventMap Map.empty Seq.empty

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
    getEvents range = lookupEventsInRange range <$> readTVar tvar
    storeEvents' uuid events = modifyTVar' tvar (\store -> storeEventMap store uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}

-- | Analog of 'tvarEventStore' for a 'GlobalStreamEventStore'
tvarGlobalStreamEventStore :: TVar (EventMap serialized) -> GlobalStreamEventStore serialized STM
tvarGlobalStreamEventStore tvar =
  let
    getGlobalEvents range = lookupGlobalEvents range <$> readTVar tvar
  in GlobalStreamEventStore{..}

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
    getEvents range = lookupEventsInRange range <$> gets getMap
    storeEvents' uuid events = modify' (modifyStore uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}
  where
    modifyStore uuid events state' =
      let
        store = getMap state'
        store' = storeEventMap store uuid events
      in setMap state' store'

-- | Analogous to 'stateEventStore' for a 'GlobalStreamEventStore'.
stateGlobalStreamEventStore
  :: (MonadState (EventMap serialized) m)
  => GlobalStreamEventStore serialized m
stateGlobalStreamEventStore = embeddedStateGlobalStreamEventStore id

-- | Analogous to 'embeddedStateEventStore' for a 'GlobalStreamEventStore'.
embeddedStateGlobalStreamEventStore
  :: (MonadState s m)
  => (s -> EventMap serialized)
  -> GlobalStreamEventStore serialized m
embeddedStateGlobalStreamEventStore getMap =
  let
    getGlobalEvents range = lookupGlobalEvents range <$> gets getMap
  in GlobalStreamEventStore{..}

lookupEventMapRaw :: EventMap serialized -> UUID -> Seq (VersionedStreamEvent serialized)
lookupEventMapRaw (EventMap uuidMap _) uuid = fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsInRange :: QueryRange UUID EventVersion -> EventMap serialized -> [VersionedStreamEvent serialized]
lookupEventsInRange (QueryRange uuid start limit) store = toList $ filterEventsByRange start' limit' 0 rawEvents
  where
    start' = unEventVersion <$> start
    limit' = unEventVersion <$> limit
    rawEvents = lookupEventMapRaw store uuid

filterEventsByRange :: QueryStart Int -> QueryLimit Int -> Int -> Seq event -> Seq event
filterEventsByRange queryStart queryLimit defaultStart events =
  let
    (start', events') =
      case queryStart of
        StartFromBeginning -> (defaultStart, events)
        StartQueryAt start -> (start, Seq.drop (start - defaultStart) events)
    events'' =
      case queryLimit of
        NoQueryLimit -> events'
        MaxNumberOfEvents num -> Seq.take num events'
        StopQueryAt stop -> Seq.take (stop - start' + 1) events'
  in events''

latestEventVersion :: EventMap serialized -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupEventMapRaw store uuid) - 1

lookupGlobalEvents :: QueryRange () SequenceNumber -> EventMap serialized -> [GlobalStreamEvent serialized]
lookupGlobalEvents (QueryRange () start limit) (EventMap _ globalEvents) = events'
  where
    start' = unSequenceNumber <$> start
    limit' = unSequenceNumber <$> limit
    events = toList $ filterEventsByRange start' limit' 1 globalEvents
    events' = zipWith (StreamEvent ()) [1..] events

storeEventMap
  :: EventMap serialized -> UUID -> [serialized] -> EventMap serialized
storeEventMap store@(EventMap uuidMap globalEvents) uuid events =
  let
    versStart = latestEventVersion store uuid + 1
    streamEvents = zipWith (StreamEvent uuid) [versStart..] events
    newMap = Map.insertWith (flip (><)) uuid (Seq.fromList streamEvents) uuidMap
    globalEvents' = globalEvents >< Seq.fromList streamEvents
  in EventMap newMap globalEvents'
