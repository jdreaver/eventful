-- | Defines an Postgresql event store.

module Eventful.Store.Postgresql
  ( PostgresqlEventStore
  , PostgresqlEventStoreT
  , postgresqlEventStore
  , initializePostgresqlEventStore
  , sqlGetGloballyOrderedEvents
  , JSONString
  , module Eventful.Store.Class
  ) where

import Control.Monad.Reader
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
    maxVersionSql = "SELECT COALESCE(MAX(version), -1) FROM events WHERE projection_id = ?"
    getAllUuidsRaw () = sqlGetProjectionIds
    getLatestVersionRaw () = sqlMaxEventVersion maxVersionSql
    getEventsRaw () uuid = sqlGetAggregateEvents uuid Nothing
    getEventsFromVersionRaw () uuid vers = sqlGetAggregateEvents uuid (Just vers)
    storeEventsRaw' () = sqlStoreEvents maxVersionSql insertMany_
    storeEventsRaw = transactionalExpectedWriteHelper getLatestVersionRaw storeEventsRaw'
  in EventStore () EventStoreDefinition{..}

initializePostgresqlEventStore :: (MonadIO m) => ConnectionPool -> m ()
initializePostgresqlEventStore pool = do
  -- Run migrations
  _ <- liftIO $ runSqlPool (runMigrationSilent migrateSqlEvent) pool

  return ()
