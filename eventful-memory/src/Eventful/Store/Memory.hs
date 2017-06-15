{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.Store.Memory
  ( memoryEventStore
  , stateEventStore
  , stateGloballyOrderedEventStore
  , EventMap
  , emptyEventMap
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

-- | An 'EventStore' that stores events in a 'TVar' and runs in 'STM'. This
-- functions initializes the store by creating the 'TVar' and hooking up the
-- event store API to that 'TVar'.
memoryEventStore :: IO (EventStore serialized STM, GloballyOrderedEventStore serialized STM)
memoryEventStore = do
  tvar <- newTVarIO emptyEventMap
  let
    getLatestVersion uuid = flip latestEventVersion uuid <$> readTVar tvar
    getEvents uuid vers = toList . (\s -> lookupEventsFromVersion s uuid vers) <$> readTVar tvar
    storeEvents' uuid events = modifyTVar' tvar (\store -> storeEventMap store uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
    getSequencedEvents seqNum = flip lookupEventMapSeq seqNum <$> readTVar tvar
  return (EventStore{..}, GloballyOrderedEventStore{..})

-- | An 'EventStore' that runs in a 'MonadState' monad.
stateEventStore
  :: (MonadState (EventMap serialized) m)
  => EventStore serialized m
stateEventStore =
  let
    getLatestVersion uuid = flip latestEventVersion uuid <$> get
    getEvents uuid vers = toList . (\s -> lookupEventsFromVersion s uuid vers) <$> get
    storeEvents' uuid events = modify (\store -> storeEventMap store uuid events)
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}

-- | A 'GloballyOrderedEventStore' that runs in a 'MonadState' monad.
stateGloballyOrderedEventStore
  :: (MonadState (EventMap serialized) m)
  => GloballyOrderedEventStore serialized m
stateGloballyOrderedEventStore =
  let
    getSequencedEvents seqNum = flip lookupEventMapSeq seqNum <$> get
  in GloballyOrderedEventStore{..}

lookupEventMapRaw :: EventMap serialized -> UUID -> Seq (StoredEvent serialized)
lookupEventMapRaw (EventMap uuidMap _) uuid =
  fmap globallyOrderedEventToStoredEvent $ fromMaybe Seq.empty $ Map.lookup uuid uuidMap

lookupEventsFromVersion :: EventMap serialized -> UUID -> Maybe EventVersion -> Seq (StoredEvent serialized)
lookupEventsFromVersion store uuid Nothing = lookupEventMapRaw store uuid
lookupEventsFromVersion store uuid (Just (EventVersion vers)) = Seq.drop vers $ lookupEventMapRaw store uuid

latestEventVersion :: EventMap serialized -> UUID -> EventVersion
latestEventVersion store uuid = EventVersion $ Seq.length (lookupEventMapRaw store uuid) - 1

lookupEventMapSeq :: EventMap serialized -> SequenceNumber -> [GloballyOrderedEvent serialized]
lookupEventMapSeq (EventMap uuidMap _) seqNum =
  sortOn globallyOrderedEventSequenceNumber $
  filter ((> seqNum) . globallyOrderedEventSequenceNumber) $
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
