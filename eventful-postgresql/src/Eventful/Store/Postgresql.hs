-- | Defines an Postgresql event store.

module Eventful.Store.Postgresql
  ( PostgresqlEventStore
  , PostgresqlEventStoreT
  , postgresqlEventStore
  , initializePostgresqlEventStore
  , sqlGetGloballyOrderedEvents
  , JSONString
  , defaultSqlEventStoreConfig
  , module Eventful.Store.Class
  ) where

import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql

import Eventful.Store.Class
import Eventful.Store.Sql

-- | The @store@ for Postgresql is currently the @Unit@ type @()@. That is, all the
-- info we need to run an Postgresql event store is presumably stored in the
-- database connection. In the future this will probably hold information like
-- what tables are used to store events, and could hold type information if we
-- allow the user to select the serialization method.
type PostgresqlEventStore m = EventStore () JSONString (SqlPersistT m)

type PostgresqlEventStoreT m = EventStoreT () JSONString (SqlPersistT m)

postgresqlEventStore :: (MonadIO m) => PostgresqlEventStore m
postgresqlEventStore =
  let
    getAllUuidsRaw () = sqlGetProjectionIds defaultSqlEventStoreConfig
    getLatestVersionRaw () = sqlMaxEventVersion defaultSqlEventStoreConfig maxPostgresVersionSql
    getEventsRaw () uuid = sqlGetAggregateEvents defaultSqlEventStoreConfig uuid Nothing
    getEventsFromVersionRaw () uuid vers = sqlGetAggregateEvents defaultSqlEventStoreConfig uuid (Just vers)
    storeEventsRaw' () = sqlStoreEvents defaultSqlEventStoreConfig maxPostgresVersionSql insertMany_
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore () EventStoreDefinition{..}

maxPostgresVersionSql :: DBName -> DBName -> DBName -> Text
maxPostgresVersionSql (DBName tableName) (DBName uuidFieldName) (DBName versionFieldName) =
  "SELECT COALESCE(MAX(" <> versionFieldName <> "), -1) FROM " <> tableName <> " WHERE " <> uuidFieldName <> " = ?"

initializePostgresqlEventStore :: (MonadIO m) => ConnectionPool -> m ()
initializePostgresqlEventStore pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  return ()
