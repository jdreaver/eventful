module EventSourcing.Store.Class
  ( RawEventStore (..)
  , SerializedEventStore (..)
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
class (Monad m) => RawEventStore m store serialized | store -> serialized where
  getUuids :: store -> m [UUID]
  getEvents :: store -> UUID -> m [StoredEvent serialized]
  storeEvents :: store -> UUID -> [serialized] -> m [StoredEvent serialized]
  latestEventVersion :: store -> UUID -> m EventVersion
  getAllEvents :: store -> SequenceNumber -> m [StoredEvent serialized]

  -- Some implementations might have a more efficient way to do this.
  getAllEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent serialized) m ())
  getAllEventsPipe store = fmap (mapM_ yield) . getAllEvents store

-- | A serialized event store can serialize/deserialize events from a raw event store.
class (RawEventStore m store serialized) => SerializedEventStore m store serialized event | store -> serialized where
  getSerializedEvents :: store -> UUID -> m [StoredEvent event]
  getAllSerializedEvents :: store -> SequenceNumber -> m [StoredEvent event]
  storeSerializedEvents :: store -> UUID -> [event] -> m [StoredEvent event]

  -- Some implementations might have a more efficient way to do this.
  getAllSerializedEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent event) m ())
  getAllSerializedEventsPipe store = fmap (mapM_ yield) . getAllSerializedEvents store

--class (SerializedEventStore m store serialized event) => TypedEventStore m store serialized event where
  -- getAggregate :: (Projection proj, Serializable (Event proj) serialized) => store -> AggregateId proj -> m proj
  -- getAggregate store (AggregateId uuid) = do
  --   events <- getEvents store uuid
  --   return $ latestProjection (mapMaybe deserialize $ storedEventEvent <$> events)

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
