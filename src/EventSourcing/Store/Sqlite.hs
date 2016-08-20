{-# LANGUAGE QuasiQuotes #-}  -- This is here so Hlint doesn't choke

-- | Defines an Sqlite event store.

module EventSourcing.Store.Sqlite
  ( PersistedSqliteEvent (..)
  , PersistedSqliteEventId
  , migrateSqliteEvent
  , getAggregateIds
  , getAggregateEvents
  , bulkInsert
  , sqliteMaxVariableNumber
  , SqliteEventStore
  , sqliteEventStore
  ) where

import Control.Monad.Reader
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List.Split (chunksOf)
import Data.Maybe (listToMaybe, mapMaybe, maybe)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import EventSourcing.Store.Class
import EventSourcing.UUID

share [mkPersist sqlSettings, mkMigrate "migrateSqliteEvent"] [persistLowerCase|
PersistedSqliteEvent
    aggregateId UUID
    eventJson ByteString
    sequenceNumber SequenceNumber
    Primary aggregateId sequenceNumber
    deriving Show
|]

getAggregateIds :: (MonadIO m) => ReaderT SqlBackend m [UUID]
getAggregateIds =
  fmap unSingle <$> rawSql "SELECT DISTINCT aggregate_id FROM persisted_sqlite_event" []

getAggregateEvents :: (FromJSON a, MonadIO m) => UUID -> ReaderT SqlBackend m [a]
getAggregateEvents uuid = do
  entities <- selectList [PersistedSqliteEventAggregateId ==. uuid] [Asc PersistedSqliteEventSequenceNumber]
  return $ mapMaybe (decode . fromStrict . persistedSqliteEventEventJson . entityVal) entities

maxSequenceNumber :: (MonadIO m) => UUID -> ReaderT SqlBackend m SequenceNumber
maxSequenceNumber uuid =
  let rawVals = rawSql "SELECT IFNULL(MAX(sequence_number), -1) FROM persisted_sqlite_event WHERE aggregate_id = ?" [toPersistValue uuid]
  in maybe 0 unSingle . listToMaybe <$> rawVals

-- | Insert all items but chunk so we don't hit SQLITE_MAX_VARIABLE_NUMBER
bulkInsert
  :: ( MonadIO m
     , PersistStore (PersistEntityBackend val)
     , PersistEntityBackend val ~ SqlBackend
     , PersistEntity val
     )
  => [val]
  -> ReaderT (PersistEntityBackend val) m [[Key val]]
bulkInsert items = forM (chunksOf sqliteMaxVariableNumber items) insertMany

-- | Search for SQLITE_MAX_VARIABLE_NUMBER here:
-- https://www.sqlite.org/limits.html
sqliteMaxVariableNumber :: Int
sqliteMaxVariableNumber = 999


data SqliteEventStore event
  = SqliteEventStore
  { _sqliteEventStoreConnectionPool :: ConnectionPool
  }

sqliteEventStore :: (MonadIO m) => ConnectionPool -> m (SqliteEventStore event)
sqliteEventStore pool = do
  -- Run migrations
  liftIO $ runSqlPool (runMigration migrateSqliteEvent) pool

  return $ SqliteEventStore pool

instance (MonadIO m, FromJSON event, ToJSON event) => EventStore (SqliteEventStore event) m event where
  getUuids = sqliteEventStoreGetUuids
  getEvents = sqliteEventStoreGetEvents
  storeEvents = sqliteEventStoreStoreEvents
  latestSequenceNumber = sqliteEventStoreLatestSequenceNumber

sqliteEventStoreGetUuids :: (MonadIO m) => SqliteEventStore event -> m [UUID]
sqliteEventStoreGetUuids (SqliteEventStore pool) =
  liftIO $ runSqlPool getAggregateIds pool

sqliteEventStoreGetEvents :: (FromJSON event, MonadIO m) => SqliteEventStore event -> UUID -> m [event]
sqliteEventStoreGetEvents (SqliteEventStore pool) uuid =
  liftIO $ runSqlPool (getAggregateEvents uuid) pool

sqliteEventStoreStoreEvents
  :: (ToJSON event, MonadIO m)
  => SqliteEventStore event -> UUID -> [event] -> m ()
sqliteEventStoreStoreEvents (SqliteEventStore pool) uuid events = do
  sequenceNum <- liftIO $ runSqlPool (maxSequenceNumber uuid) pool
  let eventsAndSeq = zip events [sequenceNum + 1..]
      entities = fmap (\(event, s) -> PersistedSqliteEvent uuid (toStrict $ encode event) s) eventsAndSeq
  liftIO $ void $ runSqlPool (bulkInsert entities) pool

sqliteEventStoreLatestSequenceNumber
  :: (MonadIO m)
  => SqliteEventStore event -> UUID -> m SequenceNumber
sqliteEventStoreLatestSequenceNumber (SqliteEventStore pool) uuid =
  liftIO $ runSqlPool (maxSequenceNumber uuid) pool
