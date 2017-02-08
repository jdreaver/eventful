{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes #-}  -- This is here so Hlint doesn't choke

-- | Defines an Sqlite event store.

module Eventful.Store.Sqlite
  ( SqliteEventStore
  , SqliteEventStoreT
  , sqliteEventStore
  , initializeSqliteEventStore
  , SqliteEvent (..)
  , SqliteEventId
  , migrateSqliteEvent
  , getProjectionIds
  , bulkInsert
  , sqliteMaxVariableNumber
  , JSONString
  , module Eventful.Store.Class
  ) where

import Control.Monad.Reader
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe, maybe)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Eventful.Store.Class
import Eventful.Store.Sqlite.Internal
import Eventful.UUID

share [mkPersist sqlSettings, mkMigrate "migrateSqliteEvent"] [persistLowerCase|
SqliteEvent sql=events
    Id SequenceNumber sql=sequence_number
    projectionId UUID
    version EventVersion
    data JSONString
    UniqueAggregateVersion projectionId version
    deriving Show
|]

-- | The @store@ for SQLite is currently the @Unit@ type @()@. That is, all the
-- info we need to run an SQLite event store is presumably stored in the
-- database connection. In the future this will probably hold information like
-- what tables are used to store events, and could hold type information if we
-- allow the user to select the serialization method.
type SqliteEventStore m = EventStore () JSONString (SqlPersistT m)

type SqliteEventStoreT m = EventStoreT () JSONString (SqlPersistT m)

sqliteEventStore :: (MonadIO m) => SqliteEventStore m
sqliteEventStore =
  EventStore () $
    EventStoreDefinition
    (const getProjectionIds)
    (const maxEventVersion)
    (\_ uuid -> getSqliteAggregateEvents uuid Nothing)
    (\_ uuid vers -> getSqliteAggregateEvents uuid (Just vers))
    (const sqliteStoreEvents)
    (const getAllEventsFromSequence)

sqliteEventToStored :: Entity SqliteEvent -> StoredEvent JSONString
sqliteEventToStored (Entity (SqliteEventKey seqNum) (SqliteEvent uuid version data')) =
  StoredEvent uuid version seqNum data'

-- sqliteEventFromSequenced :: StoredEvent event -> Entity SqliteEvent
-- sqliteEventFromSequenced (StoredEvent uuid version seqNum event) =
--   Entity (SqliteEventKey seqNum) (SqliteEvent uuid data' version)
--   where data' = toStrict (encode event)

getProjectionIds :: (MonadIO m) => ReaderT SqlBackend m [UUID]
getProjectionIds =
  fmap unSingle <$> rawSql "SELECT DISTINCT projection_id FROM events" []

getSqliteAggregateEvents :: (MonadIO m) => UUID -> Maybe EventVersion -> ReaderT SqlBackend m [StoredEvent JSONString]
getSqliteAggregateEvents uuid mVers = do
  let
    constraints =
      (SqliteEventProjectionId ==. uuid) :
      maybe [] (\vers -> [SqliteEventVersion >=. vers]) mVers
  entities <- selectList constraints [Asc SqliteEventVersion]
  return $ sqliteEventToStored <$> entities

getAllEventsFromSequence :: (MonadIO m) => SequenceNumber -> ReaderT SqlBackend m [StoredEvent JSONString]
getAllEventsFromSequence seqNum = do
  entities <- selectList [SqliteEventId >=. SqliteEventKey seqNum] [Asc SqliteEventId]
  return $ sqliteEventToStored <$> entities

maxEventVersion :: (MonadIO m) => UUID -> ReaderT SqlBackend m EventVersion
maxEventVersion uuid =
  let rawVals = rawSql "SELECT IFNULL(MAX(version), -1) FROM events WHERE projection_id = ?" [toPersistValue uuid]
  in maybe 0 unSingle . listToMaybe <$> rawVals

sqliteStoreEvents :: (MonadIO m) => UUID -> [JSONString] -> SqlPersistT m ()
sqliteStoreEvents uuid events = do
  versionNum <- maxEventVersion uuid
  let entities = zipWith (SqliteEvent uuid) [versionNum + 1..] events
  bulkInsert entities 4

-- | Insert all items but chunk so we don't hit SQLITE_MAX_VARIABLE_NUMBER
bulkInsert
  :: ( MonadIO m
     , PersistStore (PersistEntityBackend val)
     , PersistEntityBackend val ~ SqlBackend
     , PersistEntity val
     )
  => [val]
  -> Int
  -> ReaderT (PersistEntityBackend val) m ()
bulkInsert items numFields = forM_ (chunksOf chunkSize items) insertMany_
  where
    chunkSize = quot sqliteMaxVariableNumber numFields

-- | Search for SQLITE_MAX_VARIABLE_NUMBER here:
-- https://www.sqlite.org/limits.html
sqliteMaxVariableNumber :: Int
sqliteMaxVariableNumber = 999

initializeSqliteEventStore :: (MonadIO m) => ConnectionPool -> m ()
initializeSqliteEventStore pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqliteEvent) pool

  -- Create index on projection_id so retrieval is very fast
  liftIO $ runSqlPool
    (rawExecute "CREATE INDEX IF NOT EXISTS projection_id_index ON events (projection_id)" [])
    pool

  return ()
