module EventSourcing.Store.Class
  ( EventStore (..)
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

class (Monad m) => EventStore store m event | store -> event where
  getUuids :: store -> m [UUID]
  getEvents :: store -> UUID -> m [StoredEvent event]
  getAllEvents :: store -> SequenceNumber -> m [StoredEvent event]

  -- Some implementations might have a more efficient way to do this.
  getAllEventsPipe :: store -> SequenceNumber -> m (Producer (StoredEvent event) m ())
  getAllEventsPipe store = fmap (mapM_ yield) . getAllEvents store

  -- This can be made way more efficient if the implementation supports
  -- snapshots.
  getLatestProjection :: (Projection proj, Event proj ~ event) => store -> UUID -> m proj
  getLatestProjection store uuid = do
    events <- getEvents store uuid
    return $ latestProjection (storedEventEvent <$> events)

  storeEvents :: store -> UUID -> [event] -> m [StoredEvent event]
  latestEventVersion :: store -> UUID -> m EventVersion

data StoredEvent event
  = StoredEvent
  { storedEventAggregateId :: UUID
  , storedEventVersion :: EventVersion
  , storedEventSequenceNumber :: SequenceNumber
  , storedEventEvent :: event
  } deriving (Show, Read, Eq)

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
