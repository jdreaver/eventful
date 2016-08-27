module EventSourcing.Store.Class
  ( EventStore (..)
  , SequencedEventStore (..)
  , EventStoreInfo (..)
  , AggregateId (..)
  , StoredEvent (..)
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
  getEvents :: store -> AggregateId proj -> m [StoredEvent (Event proj)]
  storeEvents :: store -> AggregateId proj -> [Event proj] -> m [StoredEvent (Event proj)]
  latestEventVersion :: store -> AggregateId proj -> m EventVersion

  -- Some implementations might have a more efficient ways to do the this
  getAggregate :: (Projection proj) => store -> AggregateId proj -> m proj
  getAggregate store uuid = latestProjection . fmap storedEventEvent <$> getEvents store uuid

class (Monad m) => SequencedEventStore m store event where
  getSequencedEvents :: store -> SequenceNumber -> m [StoredEvent event]

  -- Some implementations might have a more efficient ways to do the this
  getSequencedEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent event) m ())
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
  } deriving (Show, Read, Eq, Functor)

newtype EventVersion = EventVersion { unEventVersion :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql)

newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql,
            PathPiece, ToHttpApiData, FromHttpApiData)

class (Projection proj, Monad m) => ProjectionStore m store proj | store -> proj where
  latestApplied :: store -> m SequenceNumber
  getProjection :: store -> UUID -> m proj
  applyEvents :: store -> [StoredEvent (Event proj)] -> m ()

-- data StoredProjection proj
--   = StoredProjection
--   { storedProjectionProjection :: proj
--   , storedProjectionEventVersion :: EventVersion
--   } deriving (Show)
