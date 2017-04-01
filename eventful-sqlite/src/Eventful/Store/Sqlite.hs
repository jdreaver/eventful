-- | Defines an Sqlite event store.

module Eventful.Store.Sqlite
  ( sqliteEventStore
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

sqliteEventStore
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> EventStore serialized (SqlPersistT m)
sqliteEventStore config =
  let
    getLatestVersion = sqlMaxEventVersion config maxSqliteVersionSql
    getEvents = sqlGetAggregateEvents config
    storeEvents' = sqlStoreEvents config maxSqliteVersionSql
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}

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
