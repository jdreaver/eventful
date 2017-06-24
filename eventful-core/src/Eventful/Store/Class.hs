{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.Store.Class
  ( -- * EventStore
    EventStore (..)
  , VersionedStreamEvent
  , GlobalStreamEventStore (..)
  , GlobalStreamEvent
  , ExpectedVersion (..)
  , EventWriteError (..)
  , runEventStoreUsing
  , runGlobalStreamEventStoreUsing
  , module Eventful.Store.Queries
    -- * Serialization
  , serializedEventStore
  , serializedGlobalStreamEventStore
    -- * Utility types
  , EventVersion (..)
  , SequenceNumber (..)
    -- * Utility functions
  , transactionalExpectedWriteHelper
  ) where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Web.HttpApiData
import Web.PathPieces

import Eventful.Serializer
import Eventful.Store.Queries
import Eventful.UUID

-- | The 'EventStore' is the core type of eventful. A store operates in some
-- monad @m@ and stores events by serializing them to the type @serialized@.
data EventStore serialized m
  = EventStore
  { getEvents :: QueryRange UUID EventVersion -> m [VersionedStreamEvent serialized]
    -- ^ Retrieves all the events for a given 'Projection' using that
    -- projection's UUID. If an event version is provided then all events with
    -- a version greater than or equal to that version are returned.
  , storeEvents :: ExpectedVersion -> UUID -> [serialized] -> m (Maybe EventWriteError)
    -- ^ Stores the events for a given 'Projection' using that projection's
    -- UUID.
  }

-- | Gets all the events ordered starting with a given 'SequenceNumber', and
-- ordered by 'SequenceNumber'. This is used when replaying all the events in a
-- store.
newtype GlobalStreamEventStore serialized m =
  GlobalStreamEventStore
  { getGlobalEvents :: QueryRange () SequenceNumber -> m [GlobalStreamEvent serialized]
  }

type VersionedStreamEvent serialized = StreamEvent UUID EventVersion serialized
type GlobalStreamEvent serialized = StreamEvent () SequenceNumber (VersionedStreamEvent serialized)

-- | ExpectedVersion is used to assert the event stream is at a certain version
-- number. This is used when multiple writers are concurrently writing to the
-- event store. If the expected version is incorrect, then storing fails.
data ExpectedVersion
  = AnyVersion
    -- ^ Used when the writer doesn't care what version the stream is at.
  | NoStream
    -- ^ The stream shouldn't exist yet.
  | StreamExists
    -- ^ The stream should already exist.
  | ExactVersion EventVersion
    -- ^ Used to assert the stream is at a particular version.
  deriving (Show, Eq)

data EventWriteError
  = EventStreamNotAtExpectedVersion EventVersion
  deriving (Show, Eq)

-- | Helper to create 'storeEventsRaw' given a function to get the latest
-- stream version and a function to write to the event store. **NOTE**: This
-- only works if the monad @m@ is transactional.
transactionalExpectedWriteHelper
  :: (Monad m)
  => (UUID -> m EventVersion)
  -> (UUID -> [serialized] -> m ())
  -> ExpectedVersion -> UUID -> [serialized] -> m (Maybe EventWriteError)
transactionalExpectedWriteHelper getLatestVersion' storeEvents' expected =
  go expected getLatestVersion' storeEvents'
  where
    go AnyVersion = transactionalExpectedWriteHelper' Nothing
    go NoStream = transactionalExpectedWriteHelper' (Just $ (==) (-1))
    go StreamExists = transactionalExpectedWriteHelper' (Just (> (-1)))
    go (ExactVersion vers) = transactionalExpectedWriteHelper' (Just $ (==) vers)

transactionalExpectedWriteHelper'
  :: (Monad m)
  => Maybe (EventVersion -> Bool)
  -> (UUID -> m EventVersion)
  -> (UUID -> [serialized] -> m ())
  -> UUID -> [serialized] -> m (Maybe EventWriteError)
transactionalExpectedWriteHelper' Nothing _ storeEvents' uuid events =
  storeEvents' uuid events >> return Nothing
transactionalExpectedWriteHelper' (Just f) getLatestVersion' storeEvents' uuid events = do
  latestVersion <- getLatestVersion' uuid
  if f latestVersion
  then storeEvents' uuid events >> return Nothing
  else return $ Just $ EventStreamNotAtExpectedVersion latestVersion

-- | Changes the monad an 'EventStore' runs in. This is useful to run event
-- stores in another 'Monad' while forgetting the original 'Monad'.
runEventStoreUsing
  :: (Monad m, Monad mstore)
  => (forall a. mstore a -> m a)
  -> EventStore serialized mstore
  -> EventStore serialized m
runEventStoreUsing runStore EventStore{..} =
  EventStore
  { getEvents = runStore . getEvents
  , storeEvents = \vers uuid events -> runStore $ storeEvents vers uuid events
  }

-- | Analog of 'runEventStoreUsing' for a 'GlobalStreamEventStore'.
runGlobalStreamEventStoreUsing
  :: (Monad m, Monad mstore)
  => (forall a. mstore a -> m a)
  -> GlobalStreamEventStore serialized mstore
  -> GlobalStreamEventStore serialized m
runGlobalStreamEventStoreUsing runStore GlobalStreamEventStore{..} =
  GlobalStreamEventStore
  { getGlobalEvents = runStore . getGlobalEvents
  }

-- | Wraps an 'EventStore' and transparently serializes/deserializes events for
-- you. Note that in this implementation deserialization errors when using
-- 'getEvents' are simply ignored (the event is not returned).
serializedEventStore
  :: (Monad m)
  => Serializer event serialized
  -> EventStore serialized m
  -> EventStore event m
serializedEventStore Serializer{..} store =
  EventStore
  getEvents'
  storeEvents'
  where
    getEvents' range = mapMaybe (traverse deserialize) <$> getEvents store range
    storeEvents' expectedVersion uuid events = storeEvents store expectedVersion uuid (serialize <$> events)

-- | Like 'serializedEventStore' except for 'GlobalStreamEventStore'.
serializedGlobalStreamEventStore
  :: (Monad m)
  => Serializer event serialized
  -> GlobalStreamEventStore serialized m
  -> GlobalStreamEventStore event m
serializedGlobalStreamEventStore Serializer{..} store =
  GlobalStreamEventStore getGlobalEvents'
  where
    getGlobalEvents' sequenceNumber =
      mapMaybe (traverse (traverse deserialize)) <$> getGlobalEvents store sequenceNumber

-- | Event versions are a strictly increasing series of integers for each
-- projection. They allow us to order the events when they are replayed, and
-- they also help as a concurrency check in a multi-threaded environment so
-- services modifying the projection can be sure the projection didn't change
-- during their execution.
newtype EventVersion = EventVersion { unEventVersion :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON)

-- | The sequence number gives us a global ordering of events in a particular
-- event store. Using sequence numbers is not strictly necessary for an event
-- sourcing and CQRS system, but it makes it way easier to replay events
-- consistently without having to use distributed transactions in an event bus.
-- In SQL-based event stores, they are also very cheap to create.
newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON,
            PathPiece, ToHttpApiData, FromHttpApiData)
