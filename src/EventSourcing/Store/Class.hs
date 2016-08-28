module EventSourcing.Store.Class
  ( EventStore (..)
  , SequencedEventStore (..)
  , EventStoreInfo (..)
  , AggregateId (..)
  , StoredEvent (..)
  , serializeEvent
  , deserializeEvent
  , EventVersion (..)
  , SequenceNumber (..)
  , Serializable (..)
  ) where

import Data.Aeson
import Data.Dynamic
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Pipes
import Web.HttpApiData
import Web.PathPieces

import EventSourcing.Aeson
import EventSourcing.Projection
import EventSourcing.UUID

class (Monad m) => EventStore m store proj where
  getEvents :: store -> AggregateId proj -> m [StoredEvent (Event proj)]
  storeEvents :: store -> AggregateId proj -> [Event proj] -> m [StoredEvent (Event proj)]
  latestEventVersion :: store -> AggregateId proj -> m EventVersion

  -- Some implementations might have a more efficient ways to do the this
  getAggregate :: (Projection proj) => store -> AggregateId proj -> m proj
  getAggregate store uuid = latestProjection . fmap storedEventEvent <$> getEvents store uuid

class (Monad m) => SequencedEventStore m store serialized | store -> serialized where
  getSequencedEvents :: store -> SequenceNumber -> m [StoredEvent serialized]

  -- Some implementations might have a more efficient ways to do the this
  getSequencedEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent serialized) m ())
  getSequencedEventsPipe store = fmap (mapM_ yield) . getSequencedEvents store

class (Monad m) => EventStoreInfo m store where
  getAllUuids :: store -> m [UUID]

-- | This type ensures our stored events have the correct type, but it also
-- allows us to avoid type ambiguity errors in event stores by providing the
-- phantom projection type.
newtype AggregateId proj = AggregateId { unAggregateId :: UUID }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql, FromHttpApiData)

data StoredEvent event
  = StoredEvent
  { storedEventAggregateId :: UUID
  , storedEventVersion :: EventVersion
  , storedEventSequenceNumber :: SequenceNumber
  , storedEventEvent :: event
  } deriving (Show, Eq, Functor)

serializeEvent :: (Serializable event serialized) => StoredEvent event -> StoredEvent serialized
serializeEvent = fmap serialize

deserializeEvent :: (Serializable event serialized) => StoredEvent serialized -> Maybe (StoredEvent event)
deserializeEvent (StoredEvent uuid vers seqNum event) =
  StoredEvent uuid vers seqNum <$> deserialize event

newtype EventVersion = EventVersion { unEventVersion :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql)

newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql,
            PathPiece, ToHttpApiData, FromHttpApiData)

class Serializable a b where
  serialize :: a -> b
  deserialize :: b -> Maybe a

instance (Typeable a) => Serializable a Dynamic where
  serialize = toDyn
  deserialize = fromDynamic

instance (ToJSON a, FromJSON a) => Serializable a JSONString where
  serialize = encodeJSON
  deserialize = decodeJSON
