module EventSourcing.Store.Class
  ( EventStore (..)
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

-- | An event store is anything that stores events in some order based on UUID.
class (Monad m) => EventStore store m storage | store -> storage where
  getUuids :: store -> m [UUID]
  getEvents
    :: (Serializable event storage)
    => store -> UUID -> m [StoredEvent event]
  getAllEvents
    :: (Serializable event storage)
    => store -> SequenceNumber -> m [StoredEvent event]

  -- Some implementations might have a more efficient way to do this.
  getAllEventsPipe
    :: (Serializable event storage)
    => store -> SequenceNumber -> m (Producer (StoredEvent event) m ())
  getAllEventsPipe store = fmap (mapM_ yield) . getAllEvents store

  -- This can be made way more efficient if the implementation supports
  -- snapshots.
  getLatestProjection
    :: (Projection proj, Serializable (Event proj) storage)
    => store -> UUID -> m proj
  getLatestProjection store uuid = do
    events <- getEvents store uuid
    return $ latestProjection (storedEventEvent <$> events)

  storeEvents
    :: (Serializable event storage)
    => store -> UUID -> [event] -> m [StoredEvent event]
  latestEventVersion :: store -> UUID -> m EventVersion

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

class (Projection proj, Monad m) => ProjectionStore store m proj | store -> proj where
  latestApplied :: store -> m SequenceNumber
  getProjection :: store -> UUID -> m proj
  applyEvents :: store -> [StoredEvent (Event proj)] -> m ()

-- data StoredProjection proj
--   = StoredProjection
--   { storedProjectionProjection :: proj
--   , storedProjectionEventVersion :: EventVersion
--   } deriving (Show)
