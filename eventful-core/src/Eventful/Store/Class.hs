{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.Store.Class
  ( -- * EventStore
    EventStore (..)
  , GloballyOrderedEventStore (..)
  , ExpectedVersion (..)
  , EventWriteError (..)
    -- * Utility types
  , ProjectionEvent (..)
  , StoredEvent (..)
  , storedEventToProjectionEvent
  , GloballyOrderedEvent (..)
  , globallyOrderedEventToStoredEvent
  , globallyOrderedEventToProjectionEvent
  , storedEventToGloballyOrderedEvent
  , EventVersion (..)
  , SequenceNumber (..)
    -- * Utility functions
  , transactionalExpectedWriteHelper
  ) where

import Data.Aeson
import Web.HttpApiData
import Web.PathPieces

import Eventful.UUID

-- | The 'EventStore' is the core type of eventful. A store operates in some
-- monad @m@ and stores events by serializing them to the type @serialized@.
data EventStore serialized m
  = EventStore
  { getLatestVersion :: UUID -> m EventVersion
    -- ^ Gets the latest 'EventVersion' for a given 'Projection'.
  , getEvents :: UUID -> Maybe EventVersion -> m [StoredEvent serialized]
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
newtype GloballyOrderedEventStore serialized m =
  GloballyOrderedEventStore
  { getSequencedEvents :: SequenceNumber -> m [GloballyOrderedEvent serialized]
  }

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

-- | A 'ProjectionEvent' is an event that is associated with a 'Projection' via
-- the projection's 'UUID'.
data ProjectionEvent event
  = ProjectionEvent
  { projectionEventProjectionId :: !UUID
    -- ^ The UUID of the 'Projection' that the event belongs to.
  , projectionEventEvent :: !event
    -- ^ The actual event type. Note that this can be a serialized event or the
    -- actual Haskell event type.
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | A 'StoredEvent' is an event with associated storage metadata.
data StoredEvent event
  = StoredEvent
  { storedEventProjectionId :: !UUID
    -- ^ The UUID of the 'Projection' that the event belongs to.
  , storedEventVersion :: !EventVersion
    -- ^ The version of the Projection corresponding to this event.
  , storedEventEvent :: !event
    -- ^ The actual event type. Note that this can be a serialized event or the
    -- actual Haskell event type.
  } deriving (Show, Eq, Functor, Foldable, Traversable)

storedEventToProjectionEvent :: StoredEvent event -> ProjectionEvent event
storedEventToProjectionEvent StoredEvent{..} =
  ProjectionEvent
  { projectionEventProjectionId = storedEventProjectionId
  , projectionEventEvent = storedEventEvent
  }

-- | A 'GloballyOrderedEvent' is like a 'StoredEvent' but has a global
-- 'SequenceNumber'.
data GloballyOrderedEvent event
  = GloballyOrderedEvent
  { globallyOrderedEventProjectionId :: !UUID
    -- ^ The UUID of the 'Projection' that the event belongs to.
  , globallyOrderedEventVersion :: !EventVersion
    -- ^ The version of the Projection corresponding to this event.
  , globallyOrderedEventSequenceNumber :: !SequenceNumber
    -- ^ The global sequence number of this event.
  , globallyOrderedEventEvent :: !event
    -- ^ The actual event type. Note that this can be a serialized event or the
    -- actual Haskell event type.
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Extract the 'StoredEvent' from a 'GloballyOrderedEvent'
globallyOrderedEventToStoredEvent :: GloballyOrderedEvent event -> StoredEvent event
globallyOrderedEventToStoredEvent GloballyOrderedEvent{..} =
  StoredEvent
  { storedEventProjectionId = globallyOrderedEventProjectionId
  , storedEventVersion = globallyOrderedEventVersion
  , storedEventEvent = globallyOrderedEventEvent
  }

-- | Extract the 'ProjectionEvent' from a 'GloballyOrderedEvent'
globallyOrderedEventToProjectionEvent :: GloballyOrderedEvent event -> ProjectionEvent event
globallyOrderedEventToProjectionEvent GloballyOrderedEvent{..} =
  ProjectionEvent
  { projectionEventProjectionId = globallyOrderedEventProjectionId
  , projectionEventEvent = globallyOrderedEventEvent
  }

-- | Convert a 'StoredEvent' to a 'GloballyOrderedEvent' by adding the
-- 'SequenceNumber'. This is mainly used by event stores to create globally
-- ordered events.
storedEventToGloballyOrderedEvent :: SequenceNumber -> StoredEvent event -> GloballyOrderedEvent event
storedEventToGloballyOrderedEvent sequenceNumber StoredEvent{..} =
  GloballyOrderedEvent
  { globallyOrderedEventProjectionId = storedEventProjectionId
  , globallyOrderedEventVersion = storedEventVersion
  , globallyOrderedEventSequenceNumber = sequenceNumber
  , globallyOrderedEventEvent = storedEventEvent
  }

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
