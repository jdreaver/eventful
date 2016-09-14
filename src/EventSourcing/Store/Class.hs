module EventSourcing.Store.Class
  ( EventStore (..)
  , getEvents
  , storeEvents
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
import Data.Maybe (mapMaybe)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Pipes
import Web.HttpApiData
import Web.PathPieces

import EventSourcing.Projection
import EventSourcing.UUID

class (Monad m) => EventStore m store serialized | store -> serialized where
  getAllUuids :: store -> m [UUID]
  getEventsRaw :: store -> UUID -> m [StoredEvent serialized]
  storeEventsRaw :: store -> UUID -> [serialized] -> m [StoredEvent serialized]
  latestEventVersion :: store -> UUID -> m EventVersion

  -- Some implementations might have a more efficient ways to do the this
  getAggregate :: (Projection proj, Serializable (Event proj) serialized) => store -> AggregateId proj -> m proj
  getAggregate store uuid = latestProjection . fmap storedEventEvent <$> getEvents store uuid

  getSequencedEvents :: store -> SequenceNumber -> m [StoredEvent serialized]

  -- Some implementations might have a more efficient ways to do the this
  getSequencedEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent serialized) m ())
  getSequencedEventsPipe store = fmap (mapM_ yield) . getSequencedEvents store

getEvents
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> AggregateId proj -> m [StoredEvent (Event proj)]
getEvents store (AggregateId uuid) = mapMaybe deserialize <$> getEventsRaw store uuid

storeEvents
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> AggregateId proj -> [Event proj] -> m [StoredEvent (Event proj)]
storeEvents store (AggregateId uuid) events = do
  serialized <- storeEventsRaw store uuid (serialize <$> events)
  return $ zipWith (<$) events serialized

-- | This type ensures our stored events have the correct type, but it also
-- allows us to avoid type ambiguity errors in event stores by providing the
-- phantom projection type.
newtype AggregateId proj = AggregateId { unAggregateId :: UUID }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

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

instance (Serializable a b) => Serializable (StoredEvent a) (StoredEvent b) where
  serialize = serializeEvent
  deserialize = deserializeEvent
