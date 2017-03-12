-- | Defines an Sqlite event store.

module Eventful.Store.Sqlite
  ( SqliteEventStore
  , SqliteEventStoreT
  , sqliteEventStore
  , initializeSqliteEventStore
  , module Eventful.Store.Class
  , module Eventful.Store.Sql
  ) where

import Control.Monad.Reader
import Data.Monoid
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql

import Eventful.Store.Class
import Eventful.Store.Sql

-- | A 'SqliteEventStore' holds a 'SqlEventStoreConfig', which is where the
-- store gets info about the events table.
type SqliteEventStore entity serialized m = EventStore (SqlEventStoreConfig entity serialized) serialized (SqlPersistT m)

type SqliteEventStoreT entity serialized m = EventStoreT (SqlEventStoreConfig entity serialized) serialized (SqlPersistT m)

sqliteEventStore
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> SqliteEventStore entity serialized m
sqliteEventStore config =
  let
    getAllUuidsRaw = sqlGetProjectionIds
    getLatestVersionRaw config' = sqlMaxEventVersion config' maxSqliteVersionSql
    getEventsRaw config' uuid = sqlGetAggregateEvents config' uuid Nothing
    getEventsFromVersionRaw config' uuid vers = sqlGetAggregateEvents config' uuid (Just vers)
    storeEventsRaw' config' = sqlStoreEvents config' maxSqliteVersionSql
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore config EventStoreDefinition{..}

maxSqliteVersionSql :: DBName -> DBName -> DBName -> Text
maxSqliteVersionSql (DBName tableName) (DBName uuidFieldName) (DBName versionFieldName) =
  "SELECT IFNULL(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

initializeSqliteEventStore
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> ConnectionPool
  -> m ()
initializeSqliteEventStore SqlEventStoreConfig{..} pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  -- Create index on uuid field so retrieval is very fast
  let
    (DBName tableName) = tableDBName (sqlEventStoreConfigSequenceMakeEntity undefined undefined undefined)
    (DBName uuidFieldName) = fieldDBName sqlEventStoreConfigSequenceNumberField
    indexSql =
      "CREATE INDEX IF NOT EXISTS " <>
      uuidFieldName <> "_index" <>
      " ON " <> tableName <>
      " (" <> uuidFieldName <> ")"
  liftIO $ flip runSqlPool pool $ rawExecute indexSql []

  return ()
