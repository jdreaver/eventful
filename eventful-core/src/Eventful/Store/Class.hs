module Eventful.Store.Class
  ( -- * EventStore
    EventStore (..)
  , EventStoreMetadata (..)
  , getEvents
  , getEventsFromVersion
  , storeEvents
  , storeEvent
  , storeEventsGetLatest
  , storeEventGetLatest
    -- * Utility types
  , StoredEvent (..)
  , EventVersion (..)
  , SequenceNumber (..)
  ) where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Web.HttpApiData
import Web.PathPieces

import Eventful.Projection
import Eventful.Serializable
import Eventful.UUID

class (Monad m) => EventStoreMetadata m where
  -- | Retrieves all the unique UUIDs in the event store. This is essentially a
  -- list of all the projections available in the event store.
  getAllUuids :: m [UUID]

  -- | Gets the latest 'EventVersion' for a given 'Projection'.
  getLatestVersion :: UUID -> m EventVersion

-- | The 'EventStore' is one of the core type classes of eventful. An
-- EventStore @store@ operates in some monad @m@ and serializes events using
-- the type @serialized@.
class (EventStoreMetadata m, Monad m) => EventStore m serialized | m -> serialized where
  -- | Retrieves all the events for a given 'Projection' using that
  -- projection's UUID.
  getEventsRaw :: UUID -> m [StoredEvent serialized]

  -- | Like 'getEventsRaw', but only retrieves events greater than or equal to
  -- the given version.
  getEventsFromVersionRaw :: UUID -> EventVersion -> m [StoredEvent serialized]
  getEventsFromVersionRaw uuid vers = do
    allEvents <- getEventsRaw uuid
    return $ filter ((>= vers) . storedEventVersion) allEvents

  -- | Stores the events for a given 'Projection' using that projection's UUID.
  storeEventsRaw :: UUID -> [serialized] -> m [StoredEvent serialized]

  -- | Retrieves the current state of a projection from the store. Some
  -- implementations might have a more efficient ways to do the this by using
  -- snapshots.
  getLatestProjection
    :: (Serializable proj serialized, Serializable event serialized)
    => Projection proj event -> UUID -> m proj
  getLatestProjection proj uuid = latestProjection proj . fmap storedEventEvent <$> getEvents uuid

  -- | Gets all the events ordered starting with a given 'SequenceNumber', and
  -- ordered by 'SequenceNumber'. This is used when replaying all the events in
  -- a store.
  getSequencedEvents :: SequenceNumber -> m [StoredEvent serialized]

-- | Like 'getEventsRaw', but uses a 'Serializable' instance for the event type
-- to try and deserialize them.
getEvents
  :: (Serializable event serialized, EventStore m serialized)
  => UUID -> m [StoredEvent event]
getEvents uuid = mapMaybe deserialize <$> getEventsRaw uuid

-- | Like 'getEventsFromVersionRaw', but uses a 'Serializable' instance for the
-- event type to try and deserialize them.
getEventsFromVersion
  :: (Serializable event serialized, EventStore m serialized)
  => UUID -> EventVersion -> m [StoredEvent event]
getEventsFromVersion uuid vers = mapMaybe deserialize <$> getEventsFromVersionRaw uuid vers

-- | Like 'storeEventsRaw', but uses a 'Serializable' instance for the event
-- type to serialize them.
storeEvents
  :: (Serializable event serialized, EventStore m serialized)
  => UUID -> [event] -> m [StoredEvent event]
storeEvents uuid events = do
  serialized <- storeEventsRaw uuid (serialize <$> events)
  return $ zipWith (<$) events serialized

-- | Like 'storeEvents', but just store a single event.
storeEvent
  :: (Serializable event serialized, EventStore m serialized)
  => UUID -> event -> m [StoredEvent event]
storeEvent projId event = storeEvents projId [event]

-- | Like 'storeEvents', but also return the latest projection.
storeEventsGetLatest
  :: (EventStore m serialized, Serializable proj serialized, Serializable event serialized)
  => Projection proj event -> UUID -> [event] -> m proj
storeEventsGetLatest proj projId events = do
  _ <- storeEvents projId events
  getLatestProjection proj projId

-- | Like 'storeEventsGetLatest', but only store a single event.
storeEventGetLatest
  :: (EventStore m serialized, Serializable proj serialized, Serializable event serialized)
  => Projection proj event -> UUID -> event -> m proj
storeEventGetLatest proj projId event = storeEventsGetLatest proj projId [event]

-- | A 'StoredEvent' is an event with associated storage metadata.
data StoredEvent event
  = StoredEvent
  { storedEventProjectionId :: UUID
    -- ^ The UUID of the 'Projection' that the event belongs to.
  , storedEventVersion :: EventVersion
    -- ^ The version of the Projection corresponding to this event.
  , storedEventSequenceNumber :: SequenceNumber
    -- ^ The global sequence number of this event.
  , storedEventEvent :: event
    -- ^ The actual event type. Note that this can be a serialized event or the
    -- actual Haskell event type.
  } deriving (Show, Eq, Functor)

instance (Serializable a b) => Serializable (StoredEvent a) (StoredEvent b) where
  serialize = fmap serialize
  deserialize (StoredEvent uuid vers seqNum event) =
    StoredEvent uuid vers seqNum <$> deserialize event
  deserializeEither (StoredEvent uuid vers seqNum event) =
    StoredEvent uuid vers seqNum <$> deserializeEither event

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
