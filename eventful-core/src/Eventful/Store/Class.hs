module Eventful.Store.Class
  ( -- * EventStore
    EventStore (..)
  , getEvents
  , getEventsFromVersion
  , storeEvents
  , storeEvent
  , storeEventsGetLatest
  , storeEventGetLatest
    -- * Utility types
  , ProjectionId (..)
  , projectionIdNextRandom
  , StoredEvent (..)
  , EventVersion (..)
  , SequenceNumber (..)
    -- * HasEventStore and helper functions
  , HasEventStore (..)
  , getAllUuidsM
  , getEventsRawM
  , storeEventsRawM
  , getLatestProjectionM
  , getSequencedEventsM
  , getEventsM
  , storeEventsM
  ) where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Web.HttpApiData
import Web.PathPieces

import Eventful.Projection
import Eventful.Serializable
import Eventful.UUID

-- | The 'EventStore' is one of the core type classes of eventful. An
-- EventStore @store@ operates in some monad @m@ and serializes events using
-- the type @serialized@.
class (Monad m) => EventStore m store serialized | store -> serialized where
  -- | Retrieves all the unique UUIDs in the event store. This is essentially a
  -- list of all the projections available in the event store.
  getAllUuids :: store -> m [UUID]

  -- | Retrieves all the events for a given 'Projection' using that
  -- projection's UUID.
  getEventsRaw :: store -> UUID -> m [StoredEvent serialized]

  -- | Like 'getEventsRaw', but only retrieves events greater than or equal to
  -- the given version.
  getEventsFromVersionRaw :: store -> UUID -> EventVersion -> m [StoredEvent serialized]
  getEventsFromVersionRaw store uuid vers = do
    allEvents <- getEventsRaw store uuid
    return $ filter ((>= vers) . storedEventVersion) allEvents

  -- | Stores the events for a given 'Projection' using that projection's UUID.
  storeEventsRaw :: store -> UUID -> [serialized] -> m [StoredEvent serialized]

  -- | Gets the latest 'EventVersion' for a given 'Projection'.
  getLatestVersion :: store -> UUID -> m EventVersion

  -- | Retrieves the current state of a projection from the store. Some
  -- implementations might have a more efficient ways to do the this by suing
  -- snapshots.
  getLatestProjection :: (Projection proj, Serializable (Event proj) serialized) => store -> ProjectionId proj -> m proj
  getLatestProjection store uuid = latestProjection . fmap storedEventEvent <$> getEvents store uuid

  -- | Gets all the events ordered starting with a given 'SequenceNumber', and
  -- ordered by 'SequenceNumber'. This is used when replaying all the events in
  -- a store.
  getSequencedEvents :: store -> SequenceNumber -> m [StoredEvent serialized]

-- | Like 'getEventsRaw', but uses a 'Serializable' instance for the event type
-- to try and deserialize them.
getEvents
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> ProjectionId proj -> m [StoredEvent (Event proj)]
getEvents store (ProjectionId uuid) = mapMaybe deserialize <$> getEventsRaw store uuid

-- | Like 'getEventsFromVersionRaw', but uses a 'Serializable' instance for the
-- event type to try and deserialize them.
getEventsFromVersion
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> ProjectionId proj -> EventVersion -> m [StoredEvent (Event proj)]
getEventsFromVersion store (ProjectionId uuid) vers = mapMaybe deserialize <$> getEventsFromVersionRaw store uuid vers

-- | Like 'storeEventsRaw', but uses a 'Serializable' instance for the event
-- type to serialize them.
storeEvents
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> ProjectionId proj -> [Event proj] -> m [StoredEvent (Event proj)]
storeEvents store (ProjectionId uuid) events = do
  serialized <- storeEventsRaw store uuid (serialize <$> events)
  return $ zipWith (<$) events serialized

-- | Like 'storeEvents', but just store a single event.
storeEvent
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> ProjectionId proj -> Event proj -> m [StoredEvent (Event proj)]
storeEvent store projId event = storeEvents store projId [event]

-- | Like 'storeEvents', but also return the latest projection.
storeEventsGetLatest
  :: (EventStore m store serialized, Projection proj, Serializable (Event proj) serialized)
  => store -> ProjectionId proj -> [Event proj] -> m proj
storeEventsGetLatest store projId events = do
  _ <- storeEvents store projId events
  getLatestProjection store projId

-- | Like 'storeEventsGetLatest', but only store a single event.
storeEventGetLatest
  :: (EventStore m store serialized, Projection proj, Serializable (Event proj) serialized)
  => store -> ProjectionId proj -> Event proj -> m proj
storeEventGetLatest store projId event = storeEventsGetLatest store projId [event]

-- | This type ensures our stored events have the correct type, but it also
-- allows us to avoid type ambiguity errors in event stores by providing the
-- phantom projection type.
newtype ProjectionId proj = ProjectionId { unProjectionId :: UUID }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

-- | Generate a random ProjectionId
projectionIdNextRandom :: IO (ProjectionId proj)
projectionIdNextRandom = ProjectionId <$> uuidNextRandom

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

class (EventStore m store serialized) => HasEventStore m store serialized | m -> store where
  getEventStore :: m store

getAllUuidsM :: (HasEventStore m store serialized) => m [UUID]
getAllUuidsM = getEventStore >>= getAllUuids

getEventsRawM :: (HasEventStore m store serialized) => UUID -> m [StoredEvent serialized]
getEventsRawM uuid = getEventStore >>= flip getEventsRaw uuid

storeEventsRawM :: (HasEventStore m store serialized) => UUID -> [serialized] -> m [StoredEvent serialized]
storeEventsRawM uuid events = getEventStore >>= \store -> storeEventsRaw store uuid events

getLatestProjectionM :: (Projection proj, Serializable (Event proj) serialized, HasEventStore m store serialized) => ProjectionId proj -> m proj
getLatestProjectionM pid = getEventStore >>= \store -> getLatestProjection store pid

getSequencedEventsM :: (HasEventStore m store serialized) => SequenceNumber -> m [StoredEvent serialized]
getSequencedEventsM seqNum = getEventStore >>= \store -> getSequencedEvents store seqNum

getEventsM
  :: (HasEventStore m store serialized, Serializable (Event proj) serialized)
  => ProjectionId proj -> m [StoredEvent (Event proj)]
getEventsM uuid = getEventStore >>= \store -> getEvents store uuid

storeEventsM
  :: (HasEventStore m store serialized, Serializable (Event proj) serialized)
  => ProjectionId proj -> [Event proj] -> m [StoredEvent (Event proj)]
storeEventsM uuid events = getEventStore >>= \store -> storeEvents store uuid events
