module EventSourcing.Store.Class
  ( EventStore (..)
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

-- Is the UUID part really necessary? Should that be handled in a StoredEvent
-- type?
class (Monad m) => EventStore store m event | store -> event where
  getUuids :: store -> m [UUID]
  getEvents :: store -> UUID -> m [event]
  getAllEvents :: store -> SequenceNumber -> m [(UUID, event)]

  getAllEventsPipe :: store -> SequenceNumber -> m (Producer (UUID, event) m ())
  getAllEventsPipe store = fmap (mapM_ yield) . getAllEvents store

  storeEvents :: store -> UUID -> [event] -> m ()
  latestEventVersion :: store -> UUID -> m EventVersion

-- data StoredEvent event
--   = StoredEvent
--   { storedEventEvent :: event
--   , storedEventEventVersion :: EventVersion
--   , storedEventTime :: UTCTime
--   }

newtype EventVersion = EventVersion { unEventVersion :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql)

newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON, PersistField, PersistFieldSql,
            PathPiece, ToHttpApiData, FromHttpApiData)

class (Projection proj, Monad m) => ProjectionStore store m proj | store -> proj where
  latestApplied :: store -> m SequenceNumber
  getProjection :: store -> UUID -> m proj
  applyEvents :: store -> UUID -> [Event proj] -> m ()

-- data StoredProjection proj
--   = StoredProjection
--   { storedProjectionProjection :: proj
--   , storedProjectionEventVersion :: EventVersion
--   } deriving (Show)
