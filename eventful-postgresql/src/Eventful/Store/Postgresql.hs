-- | Defines an Postgresql event store.

module Eventful.Store.Postgresql
  ( PostgresqlEventStore
  , PostgresqlEventStoreT
  , postgresqlEventStore
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

-- | A 'PostgresqlEventStore' holds a 'SqlEventStoreConfig', which is where the
-- store gets info about the events table.
type PostgresqlEventStore entity serialized m = EventStore (SqlEventStoreConfig entity serialized) serialized (SqlPersistT m)

type PostgresqlEventStoreT entity serialized m = EventStoreT (SqlEventStoreConfig entity serialized) serialized (SqlPersistT m)

postgresqlEventStore
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> PostgresqlEventStore entity serialized m
postgresqlEventStore config =
  let
    getAllUuidsRaw = sqlGetProjectionIds
    getLatestVersionRaw config' = sqlMaxEventVersion config' maxPostgresVersionSql
    getEventsRaw config' uuid = sqlGetAggregateEvents config' uuid Nothing
    getEventsFromVersionRaw config' uuid vers = sqlGetAggregateEvents config' uuid (Just vers)
    storeEventsRaw' config' = sqlStoreEvents config' maxPostgresVersionSql insertMany_
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore config EventStoreDefinition{..}

maxPostgresVersionSql :: DBName -> DBName -> DBName -> Text
maxPostgresVersionSql (DBName tableName) (DBName uuidFieldName) (DBName versionFieldName) =
  "SELECT COALESCE(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

initializePostgresqlEventStore :: (MonadIO m) => ConnectionPool -> m ()
initializePostgresqlEventStore pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  return ()
