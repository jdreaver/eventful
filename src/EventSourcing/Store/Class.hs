module EventSourcing.Store.Class
  ( EventStore (..)
  , SequencedEventStore (..)
  , EventStoreInfo (..)
  , AggregateId (..)
  , StoredEvent (..)
  , DynamicStoredEvent (..)
  , dynamicEventToStored
  , storedEventToDynamic
  , EventVersion (..)
  , SequenceNumber (..)
  , ProjectionStore (..)
  ) where

import Data.Aeson
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Pipes
import Web.HttpApiData
import Web.PathPieces

import EventSourcing.Projection
import EventSourcing.UUID

class (Monad m) => EventStore m store proj where
  getEvents :: store -> AggregateId proj -> m [StoredEvent proj]
  storeEvents :: store -> AggregateId proj -> [Event proj] -> m [StoredEvent proj]
  latestEventVersion :: store -> AggregateId proj -> m EventVersion

  -- Some implementations might have a more efficient ways to do the this
  getAggregate :: (Projection proj) => store -> AggregateId proj -> m proj
  getAggregate store uuid = latestProjection . fmap storedEventEvent <$> getEvents store uuid

class (Monad m) => SequencedEventStore m store serialized | store -> serialized where
  getSequencedEvents :: store -> SequenceNumber -> m [DynamicStoredEvent serialized]

  -- Some implementations might have a more efficient ways to do the this
  getSequencedEventsPipe :: store -> SequenceNumber -> m (Producer (DynamicStoredEvent serialized) m ())
  getSequencedEventsPipe store = fmap (mapM_ yield) . getSequencedEvents store

class (Monad m) => EventStoreInfo m store where
  getAllUuids :: store -> m [UUID]

-- | This type ensures our stored events have the correct type, but it also
-- allows us to avoid type ambiguity errors in event stores by providing the
-- phantom projection type.
newtype AggregateId proj = AggregateId { unAggregateId :: UUID }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, PersistField, PersistFieldSql, FromHttpApiData)

data StoredEvent proj
  = StoredEvent
  { storedEventAggregateId :: AggregateId proj
  , storedEventVersion :: EventVersion
  , storedEventSequenceNumber :: SequenceNumber
  , storedEventEvent :: Event proj
  }

data DynamicStoredEvent a
  = DynamicStoredEvent
  { dynamicStoredEventAggregateId :: UUID
  , dynamicStoredEventVersion :: EventVersion
  , dynamicStoredEventSequenceNumber :: SequenceNumber
  , dynamicStoredEventEvent :: a
  } deriving (Show, Eq, Functor)

dynamicEventToStored :: (serialized -> Maybe (Event proj)) -> DynamicStoredEvent serialized -> Maybe (StoredEvent proj)
dynamicEventToStored deserialize (DynamicStoredEvent uuid vers seqNum event) =
  StoredEvent (AggregateId uuid) vers seqNum <$> deserialize event

storedEventToDynamic :: (Event proj -> serialized) -> StoredEvent proj -> DynamicStoredEvent serialized
storedEventToDynamic serialize (StoredEvent (AggregateId uuid) vers seqNum event) =
  DynamicStoredEvent uuid vers seqNum (serialize event)

newtype EventVersion = EventVersion { unEventVersion :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql)

newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql,
            PathPiece, ToHttpApiData, FromHttpApiData)

class (Projection proj, Monad m) => ProjectionStore m store proj | store -> proj where
  latestApplied :: store -> m SequenceNumber
  getProjection :: store -> UUID -> m proj
  applyEvents :: store -> [StoredEvent proj] -> m ()

-- data StoredProjection proj
--   = StoredProjection
--   { storedProjectionProjection :: proj
--   , storedProjectionEventVersion :: EventVersion
--   } deriving (Show)
