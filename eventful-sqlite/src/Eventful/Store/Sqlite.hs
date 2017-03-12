-- | Defines an Sqlite event store.

module Eventful.Store.Sqlite
  ( SqliteEventStore
  , SqliteEventStoreT
  , sqliteEventStore
  , initializeSqliteEventStore
  , bulkInsert
  , sqliteMaxVariableNumber
  , sqlGetGloballyOrderedEvents
  , JSONString
  , defaultSqlEventStoreConfig
  , module Eventful.Store.Class
  ) where

import Control.Monad.Reader
import Data.List.Split (chunksOf)
import Data.Monoid
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql

import Eventful.Store.Class
import Eventful.Store.Sql

-- | The @store@ for SQLite is currently the @Unit@ type @()@. That is, all the
-- info we need to run an SQLite event store is presumably stored in the
-- database connection. In the future this will probably hold information like
-- what tables are used to store events, and could hold type information if we
-- allow the user to select the serialization method.
type SqliteEventStore m = EventStore () JSONString (SqlPersistT m)

type SqliteEventStoreT m = EventStoreT () JSONString (SqlPersistT m)

sqliteEventStore :: (MonadIO m) => SqliteEventStore m
sqliteEventStore =
  let
    getAllUuidsRaw () = sqlGetProjectionIds defaultSqlEventStoreConfig
    getLatestVersionRaw () = sqlMaxEventVersion defaultSqlEventStoreConfig maxSqliteVersionSql
    getEventsRaw () uuid = sqlGetAggregateEvents defaultSqlEventStoreConfig uuid Nothing
    getEventsFromVersionRaw () uuid vers = sqlGetAggregateEvents defaultSqlEventStoreConfig uuid (Just vers)
    storeEventsRaw' () = sqlStoreEvents defaultSqlEventStoreConfig maxSqliteVersionSql (bulkInsert 4)
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore () EventStoreDefinition{..}

maxSqliteVersionSql :: DBName -> DBName -> DBName -> Text
maxSqliteVersionSql (DBName tableName) (DBName uuidFieldName) (DBName versionFieldName) =
  "SELECT IFNULL(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

-- | Insert all items but chunk so we don't hit SQLITE_MAX_VARIABLE_NUMBER
bulkInsert
  :: ( MonadIO m
     , PersistStore (PersistEntityBackend val)
     , PersistEntityBackend val ~ SqlBackend
     , PersistEntity val
     )
  => Int
  -> [val]
  -> ReaderT (PersistEntityBackend val) m ()
bulkInsert numFields items = forM_ (chunksOf chunkSize items) insertMany_
  where
    chunkSize = quot sqliteMaxVariableNumber numFields

-- | Search for SQLITE_MAX_VARIABLE_NUMBER here:
-- https://www.sqlite.org/limits.html
sqliteMaxVariableNumber :: Int
sqliteMaxVariableNumber = 999

initializeSqliteEventStore :: (MonadIO m) => ConnectionPool -> m ()
initializeSqliteEventStore pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  -- Create index on projection_id so retrieval is very fast
  let
    uuidColumn = unDBName $ fieldDBName (sqlEventStoreConfigUUIDField defaultSqlEventStoreConfig)
    indexSql =
      "CREATE INDEX IF NOT EXISTS " <>
      uuidColumn <> "_index" <>
      " ON events (" <>
      uuidColumn <>
      ")"
  liftIO $ flip runSqlPool pool $ rawExecute indexSql []

  return ()
