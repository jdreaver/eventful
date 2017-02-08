module Eventful.Store.Class
  ( -- * EventStore
    EventStore (..)
  , EventStoreDefinition (..)
  , EventStoreT (..)
  , ExpectedVersion (..)
  , EventWriteError (..)
  , runEventStore
  , getAllUuids
  , getLatestVersion
  , getEvents
  , getEventsFromVersion
  , storeEvents
  , storeEvent
  , getSequencedEvents
  , getLatestProjection
    -- * Utility types
  , StoredEvent (..)
  , EventVersion (..)
  , SequenceNumber (..)
    -- * Utility functions
  , transactionalExpectedWriteHelper
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Maybe (mapMaybe)
import Web.HttpApiData
import Web.PathPieces

import Eventful.Projection
import Eventful.Serializable
import Eventful.UUID

-- | The 'EventStore' is the core type of eventful. A @store@ operates in some
-- monad @m@ and stores events by serializing them to the type @serialized@.
data EventStore store serialized m
  = EventStore
  { eventStoreStore :: store
  , eventStoreDefinition :: EventStoreDefinition store serialized m
  }

-- | An 'EventStoreDefinition' defines how a @store@ performs the operations
-- expected of an 'EventStore'. This is where you define new event stores.
data EventStoreDefinition store serialized m
  = EventStoreDefinition
  { getAllUuidsRaw :: store -> m [UUID]
    -- ^ Retrieves all the unique UUIDs in the event store. This is essentially
    -- a list of all the projections available in the event store.
  , getLatestVersionRaw :: store -> UUID -> m EventVersion
    -- ^ Gets the latest 'EventVersion' for a given 'Projection'.
  , getEventsRaw :: store -> UUID -> m [StoredEvent serialized]
    -- ^ Retrieves all the events for a given 'Projection' using that
    -- projection's UUID.
  , getEventsFromVersionRaw :: store -> UUID -> EventVersion -> m [StoredEvent serialized]
    -- ^ Like 'getEventsRaw', but only retrieves events greater than or equal
    -- to the given version.
  , storeEventsRaw :: store -> ExpectedVersion -> UUID -> [serialized] -> m (Maybe EventWriteError)
    -- ^ Stores the events for a given 'Projection' using that projection's
    -- UUID.
  , getSequencedEventsRaw :: store -> SequenceNumber -> m [StoredEvent serialized]
    -- ^ Gets all the events ordered starting with a given 'SequenceNumber',
    -- and ordered by 'SequenceNumber'. This is used when replaying all the
    -- events in a store.
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
  => (store -> UUID -> m EventVersion)
  -> (store -> UUID -> [serialized] -> m ())
  -> store -> ExpectedVersion -> UUID -> [serialized] -> m (Maybe EventWriteError)
transactionalExpectedWriteHelper getLatestVersion' storeEvents' store expected =
  go expected getLatestVersion' storeEvents' store
  where
    go AnyVersion = transactionalExpectedWriteHelper' Nothing
    go NoStream = transactionalExpectedWriteHelper' (Just $ (==) (-1))
    go StreamExists = transactionalExpectedWriteHelper' (Just (> (-1)))
    go (ExactVersion vers) = transactionalExpectedWriteHelper' (Just $ (==) vers)

transactionalExpectedWriteHelper'
  :: (Monad m)
  => Maybe (EventVersion -> Bool)
  -> (store -> UUID -> m EventVersion)
  -> (store -> UUID -> [serialized] -> m ())
  -> store  -> UUID -> [serialized] -> m (Maybe EventWriteError)
transactionalExpectedWriteHelper' Nothing _ storeEvents' store uuid events =
  storeEvents' store uuid events >> return Nothing
transactionalExpectedWriteHelper' (Just f) getLatestVersion' storeEvents' store uuid events = do
  latestVersion <- getLatestVersion' store uuid
  if f latestVersion
  then storeEvents' store uuid events >> return Nothing
  else return $ Just $ EventStreamNotAtExpectedVersion latestVersion

-- | Monad to run event store actions in. It us just a newtype around 'ReaderT'
-- that holds an 'EventStore' and uses @m@ as the base monad.
--
-- Note that there is a 'MonadTrans' instance, so if you have some action you
-- want to perform in the base monad @m@ you can just 'lift' it.
newtype EventStoreT store serialized m a
  = EventStoreT { unEventStoreT :: ReaderT (EventStore store serialized m) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (EventStoreT store serialized) where
  lift = EventStoreT . lift

-- | Run an action in the 'EventStoreT' monad. This uses the passed
-- 'EventStore' to take your event store action and return an action in the
-- base event store monad.
runEventStore :: EventStore store serialized m -> EventStoreT store serialized m a -> m a
runEventStore store (EventStoreT action) = runReaderT action store

askEventStore :: (Monad m) => EventStoreT store serialized m (EventStore store serialized m)
askEventStore = EventStoreT ask

-- | Convenience wrapper around 'getAllUuidsRaw'
getAllUuids :: (Monad m) => EventStoreT store serialized m [UUID]
getAllUuids = do
  EventStore store EventStoreDefinition{..} <- askEventStore
  lift $ getAllUuidsRaw store

-- | Convenience wrapper around 'getLatestVersion'
getLatestVersion :: (Monad m) => UUID -> EventStoreT store serialized m EventVersion
getLatestVersion uuid = do
  EventStore store EventStoreDefinition{..} <- askEventStore
  lift $ getLatestVersionRaw store uuid

-- | Like 'getEventsRaw', but uses a 'Serializable' instance for the event type
-- to try and deserialize them.
getEvents
  :: (Monad m, Serializable event serialized)
  => UUID -> EventStoreT store serialized m [StoredEvent event]
getEvents uuid = do
  EventStore store EventStoreDefinition{..} <- askEventStore
  lift $ mapMaybe deserialize <$> getEventsRaw store uuid

-- | Like 'getEventsFromVersionRaw', but uses a 'Serializable' instance for the
-- event type to try and deserialize them.
getEventsFromVersion
  :: (Monad m, Serializable event serialized)
  => UUID -> EventVersion -> EventStoreT store serialized m [StoredEvent event]
getEventsFromVersion uuid vers = do
  EventStore store EventStoreDefinition{..} <- askEventStore
  lift $ mapMaybe deserialize <$> getEventsFromVersionRaw store uuid vers

-- | Like 'storeEventsRaw', but uses a 'Serializable' instance for the event
-- type to serialize them.
storeEvents
  :: (Monad m, Serializable event serialized)
  => ExpectedVersion -> UUID -> [event] -> EventStoreT store serialized m (Maybe EventWriteError)
storeEvents expectedVersion uuid events = do
  EventStore store EventStoreDefinition{..} <- askEventStore
  lift $ storeEventsRaw store expectedVersion uuid (serialize <$> events)

-- | Like 'storeEvents', but just store a single event.
storeEvent
  :: (Monad m, Serializable event serialized)
  => ExpectedVersion -> UUID -> event -> EventStoreT store serialized m (Maybe EventWriteError)
storeEvent expectedVersion projId event = storeEvents expectedVersion projId [event]

-- | Convenience wrapper around 'getSequencedEventsRaw'
getSequencedEvents :: (Monad m) => SequenceNumber -> EventStoreT store serialized m [StoredEvent serialized]
getSequencedEvents seqNum = do
  EventStore store EventStoreDefinition{..} <- askEventStore
  lift $ getSequencedEventsRaw store seqNum

-- | Gets the latest projection from a store using 'getEvents'
getLatestProjection
  :: (Monad m, Serializable event serialized)
  => Projection proj event -> UUID -> EventStoreT store serialized m (proj, EventVersion)
getLatestProjection proj uuid = do
  events <- getEvents uuid
  let
    latestVersion = maxEventVersion events
    latestProj = latestProjection proj $ storedEventEvent <$> events
  return (latestProj, latestVersion)
  where
    maxEventVersion [] = -1
    maxEventVersion es = maximum $ storedEventVersion <$> es

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
