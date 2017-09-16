{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Defines an Sqlite event store.

module Eventful.Store.Sqlite
  ( sqliteEventStoreWriter
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

-- | An 'EventStoreWriter' that uses an SQLite database as a backend. Use
-- 'SqlEventStoreConfig' to configure this event store.
sqliteEventStoreWriter
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> VersionedEventStoreWriter (SqlPersistT m) serialized
sqliteEventStoreWriter config = EventStoreWriter $ transactionalExpectedWriteHelper getLatestVersion storeEvents'
  where
    getLatestVersion = sqlMaxEventVersion config maxSqliteVersionSql
    storeEvents' = sqlStoreEvents config Nothing maxSqliteVersionSql

maxSqliteVersionSql :: DBName -> DBName -> DBName -> Text
maxSqliteVersionSql (DBName tableName) (DBName uuidFieldName) (DBName versionFieldName) =
  "SELECT IFNULL(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

-- | This functions runs the migrations required to create the events table and
-- also adds an index on the UUID column.
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
