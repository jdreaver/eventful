-- | Defines an Postgresql event store.

module Eventful.Store.Postgresql
  ( postgresqlEventStore
  , initializePostgresqlEventStore
  , module Eventful.Store.Class
  , module Eventful.Store.Sql
  ) where

import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql

import Eventful.Store.Class
import Eventful.Store.Sql

-- | An 'EventStore' that uses a PostgreSQL database as a backend. Use
-- 'SqlEventStoreConfig' to configure this event store.
postgresqlEventStore
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> EventStore serialized (SqlPersistT m)
postgresqlEventStore config =
  let
    getLatestVersion = sqlMaxEventVersion config maxPostgresVersionSql
    getEvents = sqlGetAggregateEvents config
    storeEvents' = sqlStoreEvents config maxPostgresVersionSql
    storeEvents = transactionalExpectedWriteHelper getLatestVersion storeEvents'
  in EventStore{..}

maxPostgresVersionSql :: DBName -> DBName -> DBName -> Text
maxPostgresVersionSql (DBName tableName) (DBName uuidFieldName) (DBName versionFieldName) =
  "SELECT COALESCE(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

-- | This function runs migrations to create the events table if it isn't
-- present.
initializePostgresqlEventStore :: (MonadIO m) => ConnectionPool -> m ()
initializePostgresqlEventStore pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  return ()
