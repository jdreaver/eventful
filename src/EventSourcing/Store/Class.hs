module EventSourcing.Store.Class
  ( RawEventStore (..)
  , SequencedEventStore (..)
  , TypedEventStore (..)
  , AggregateId (..)
  , Serializable (..)
  , StoredEvent (..)
  , EventVersion (..)
  , SequenceNumber (..)
  , ProjectionStore (..)
  ) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Pipes
import Web.HttpApiData
import Web.PathPieces

import EventSourcing.Projection
import EventSourcing.UUID

-- | The 'Serializable' class is used to abstract away the common parts of
-- event storage.
class Serializable a b where
  serialize :: a -> b
  deserialize :: b -> Maybe a

instance (a ~ b) => Serializable a b where
  serialize = id
  deserialize = Just

instance (FromJSON a, ToJSON a) => Serializable a ByteString where
  serialize = encode
  deserialize = decode

-- | An raw event store is anything that stores serialized events in some order
-- based on UUID. This class knows nothing about Projections or how the events
-- are used, it just marshals them around.
class (Monad m) => RawEventStore m store where
  getUuids :: store -> m [UUID]
  getEvents :: store -> UUID -> m [StoredEvent event]
  storeEvents :: store -> UUID -> [event] -> m [StoredEvent event]

-- | An event store with some global ordering of events. This is very useful to
-- help synchronize and/or replay read models without concurrency concerns.
class (Monad m) => SequencedEventStore store m where
  getAllEvents :: store -> SequenceNumber -> m [StoredEvent event]

  -- Some implementations might have a more efficient way to do this.
  getAllEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent event) m ())
  getAllEventsPipe store = fmap (mapM_ yield) . getAllEvents store

class (Monad m, RawEventStore m store) => TypedEventStore m store where
  getAggregateEvents :: store -> AggregateId proj -> m [StoredEvent (Event proj)]
  storeAggregateEvents :: store -> AggregateId proj -> [Event proj] -> m [StoredEvent (Event proj)]

  getAggregate :: (Projection proj) => store -> AggregateId proj -> m proj
  getAggregate store (AggregateId uuid) = do
    events <- getEvents store uuid
    return $ latestProjection (storedEventEvent <$> events)

-- | This type ensures our stored events have the correct type, but it also
-- allows us to avoid type ambiguity errors in event stores by providing the
-- phantom projection type.
newtype AggregateId proj = AggregateId { unAggregateId :: UUID }
  deriving (Show, Eq, ToJSON, FromJSON, PersistField, PersistFieldSql, FromHttpApiData)

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
