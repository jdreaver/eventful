{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.Store.Class
  ( -- * EventStore
    EventStoreReader (..)
  , EventStoreWriter (..)
  , VersionedEventStoreReader
  , GlobalEventStoreReader
  , VersionedEventStoreWriter
  , StreamEvent (..)
  , VersionedStreamEvent
  , GlobalStreamEvent
  , ExpectedPosition (..)
  , EventWriteError (..)
  , runEventStoreReaderUsing
  , runEventStoreWriterUsing
  , module Eventful.Store.Queries
    -- * Serialization
  , serializedEventStoreReader
  , serializedVersionedEventStoreReader
  , serializedGlobalEventStoreReader
  , serializedEventStoreWriter
    -- * Utility types
  , EventVersion (..)
  , SequenceNumber (..)
    -- * Utility functions
  , transactionalExpectedWriteHelper
  ) where

import Data.Aeson
import Data.Functor.Contravariant
import Data.Maybe (mapMaybe)
import Web.HttpApiData
import Web.PathPieces

import Eventful.Serializer
import Eventful.Store.Queries
import Eventful.UUID

-- | An 'EventStoreReader' is a function to query a stream from an event store.
-- It operates in some monad @m@ and returns events of type @event@ from a
-- stream at @key@ ordered by @position@.
newtype EventStoreReader key position m event = EventStoreReader { getEvents :: QueryRange key position -> m [event] }

instance (Functor m) => Functor (EventStoreReader key position m) where
  fmap f (EventStoreReader reader) = EventStoreReader $ fmap (fmap f) <$> reader

type VersionedEventStoreReader m event = EventStoreReader UUID EventVersion m (VersionedStreamEvent event)
type GlobalEventStoreReader m event = EventStoreReader () SequenceNumber m (GlobalStreamEvent event)

-- | An 'EventStoreWriter' is a function to write some events of type @event@
-- to an event store in some monad @m@.
newtype EventStoreWriter key position m event
  = EventStoreWriter { storeEvents :: key -> ExpectedPosition position -> [event] -> m (Either (EventWriteError position) EventVersion) }

instance Contravariant (EventStoreWriter key position m) where
  contramap f (EventStoreWriter writer) = EventStoreWriter $ \vers uuid -> writer vers uuid . fmap f

type VersionedEventStoreWriter = EventStoreWriter UUID EventVersion

-- | An event along with the @key@ for the event stream it is from and its
-- @position@ in that event stream.
data StreamEvent key position event
  = StreamEvent
  { streamEventKey :: !key
  , streamEventPosition :: !position
  , streamEventEvent :: !event
  } deriving (Show, Eq, Functor, Foldable, Traversable)

type VersionedStreamEvent event = StreamEvent UUID EventVersion event
type GlobalStreamEvent event = StreamEvent () SequenceNumber (VersionedStreamEvent event)

-- | ExpectedPosition is used to assert the event stream is at a certain
-- position. This is used when multiple writers are concurrently writing to the
-- event store. If the expected position is incorrect, then storing fails.
data ExpectedPosition position
  = AnyPosition
    -- ^ Used when the writer doesn't care what position the stream is at.
  | NoStream
    -- ^ The stream shouldn't exist yet.
  | StreamExists
    -- ^ The stream should already exist.
  | ExactPosition position
    -- ^ Used to assert the stream is at a particular position.
  deriving (Show, Eq)

data EventWriteError position
  = EventStreamNotAtExpectedVersion position
  deriving (Show, Eq)

-- | Helper to create 'storeEventsRaw' given a function to get the latest
-- stream version and a function to write to the event store. **NOTE**: This
-- only works if the monad @m@ is transactional.
transactionalExpectedWriteHelper
  :: (Monad m, Ord position, Num position)
  => (key -> m position)
  -> (key -> [event] -> m EventVersion)
  -> key -> ExpectedPosition position -> [event] -> m (Either (EventWriteError position) EventVersion)
transactionalExpectedWriteHelper getLatestVersion' storeEvents' key expected =
  go expected getLatestVersion' storeEvents' key
  where
    go AnyPosition = transactionalExpectedWriteHelper' Nothing
    go NoStream = transactionalExpectedWriteHelper' (Just $ (==) (-1))
    go StreamExists = transactionalExpectedWriteHelper' (Just (> (-1)))
    go (ExactPosition pos) = transactionalExpectedWriteHelper' (Just $ (==) pos)

transactionalExpectedWriteHelper'
  :: (Monad m)
  => Maybe (position -> Bool)
  -> (key -> m position)
  -> (key -> [event] -> m EventVersion)
  -> key -> [event] -> m (Either (EventWriteError position) EventVersion)
transactionalExpectedWriteHelper' Nothing _ storeEvents' uuid events =
  storeEvents' uuid events >>= return . Right
transactionalExpectedWriteHelper' (Just f) getLatestVersion' storeEvents' uuid events = do
  latestVersion <- getLatestVersion' uuid
  if f latestVersion
  then storeEvents' uuid events >>= return . Right
  else return $ Left $ EventStreamNotAtExpectedVersion latestVersion

-- | Changes the monad an 'EventStoreReader' runs in. This is useful to run
-- event stores in another 'Monad' while forgetting the original 'Monad'.
runEventStoreReaderUsing
  :: (Monad m, Monad mstore)
  => (forall a. mstore a -> m a)
  -> EventStoreReader key position mstore event
  -> EventStoreReader key position m event
runEventStoreReaderUsing runStore (EventStoreReader f) = EventStoreReader (runStore . f)

-- | Analog of 'runEventStoreReaderUsing' for a 'EventStoreWriter'.
runEventStoreWriterUsing
  :: (Monad m, Monad mstore)
  => (forall a. mstore a -> m a)
  -> EventStoreWriter key posirion mstore event
  -> EventStoreWriter key posirion m event
runEventStoreWriterUsing runStore (EventStoreWriter f) =
  EventStoreWriter $ \vers uuid events -> runStore $ f vers uuid events

-- | Wraps an 'EventStoreReader' and transparently serializes/deserializes
-- events for you. Note that in this implementation deserialization errors are
-- simply ignored (the event is not returned).
serializedEventStoreReader
  :: (Monad m)
  => Serializer event serialized
  -> EventStoreReader key position m serialized
  -> EventStoreReader key position m event
serializedEventStoreReader Serializer{..} (EventStoreReader reader) =
  EventStoreReader $ fmap (mapMaybe deserialize) . reader

-- | Convenience wrapper around 'serializedEventStoreReader' for
-- 'VersionedEventStoreReader'.
serializedVersionedEventStoreReader
  :: (Monad m)
  => Serializer event serialized
  -> VersionedEventStoreReader m serialized
  -> VersionedEventStoreReader m event
serializedVersionedEventStoreReader serializer = serializedEventStoreReader (traverseSerializer serializer)

-- | Convenience wrapper around 'serializedEventStoreReader' for
-- 'GlobalEventStoreReader'.
serializedGlobalEventStoreReader
  :: (Monad m)
  => Serializer event serialized
  -> GlobalEventStoreReader m serialized
  -> GlobalEventStoreReader m event
serializedGlobalEventStoreReader serializer = serializedEventStoreReader (traverseSerializer (traverseSerializer serializer))

-- | Like 'serializedEventStoreReader' but for an 'EventStoreWriter'. Note that
-- 'EventStoreWriter' is an instance of 'Contravariant', so you can just use
-- @contramap serialize@ instead of this function.
serializedEventStoreWriter
  :: (Monad m)
  => Serializer event serialized
  -> EventStoreWriter key position m serialized
  -> EventStoreWriter key position m event
serializedEventStoreWriter Serializer{..} = contramap serialize

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
