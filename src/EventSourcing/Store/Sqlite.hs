{-# LANGUAGE QuasiQuotes #-}  -- This is here so Hlint doesn't choke

-- | Defines an Sqlite event store.

module EventSourcing.Store.Sqlite
  ( SqliteEvent (..)
  , SqliteEventId
  , migrateSqliteEvent
  , getAggregateIds
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

import EventSourcing.Projection
import EventSourcing.Store.Class
import EventSourcing.UUID

share [mkPersist sqlSettings, mkMigrate "migrateSqliteEvent"] [persistLowerCase|
SqliteEvent sql=events
    Id SequenceNumber sql=sequence_number
    aggregateId UUID
    data ByteString
    version EventVersion
    UniqueAggregateVersion aggregateId version
    deriving Show
|]

sqliteEventToStored :: Entity SqliteEvent -> StoredEvent ByteString
sqliteEventToStored (Entity (SqliteEventKey seqNum) (SqliteEvent uuid data' version)) =
  StoredEvent uuid version seqNum data'

-- sqliteEventFromStored :: (Serializable event ByteString) => StoredEvent event -> Entity SqliteEvent
-- sqliteEventFromStored (StoredEvent uuid version seqNum event) =
--   Entity (SqliteEventKey seqNum) (SqliteEvent uuid data' version)
--   where data' = toStrict (encode event)

getAggregateIds :: (MonadIO m) => ReaderT SqlBackend m [UUID]
getAggregateIds =
  fmap unSingle <$> rawSql "SELECT DISTINCT aggregate_id FROM events" []

getSqliteAggregateEvents :: (MonadIO m) => UUID -> ReaderT SqlBackend m [StoredEvent ByteString]
getSqliteAggregateEvents uuid = do
  entities <- selectList [SqliteEventAggregateId ==. uuid] [Asc SqliteEventVersion]
  return $ sqliteEventToStored <$> entities

getAllEventsFromSequence :: (MonadIO m) => SequenceNumber -> ReaderT SqlBackend m [StoredEvent ByteString]
getAllEventsFromSequence seqNum = do
  entities <- selectList [SqliteEventId >=. SqliteEventKey seqNum] [Asc SqliteEventId]
  return $ sqliteEventToStored <$> entities

maxEventVersion :: (MonadIO m) => UUID -> ReaderT SqlBackend m EventVersion
maxEventVersion uuid =
  let rawVals = rawSql "SELECT IFNULL(MAX(version), -1) FROM events WHERE aggregate_id = ?" [toPersistValue uuid]
  in maybe 0 unSingle . listToMaybe <$> rawVals

-- | Insert all items but chunk so we don't hit SQLITE_MAX_VARIABLE_NUMBER
bulkInsert
  :: ( MonadIO m
     , PersistStore (PersistEntityBackend val)
     , PersistEntityBackend val ~ SqlBackend
     , PersistEntity val
     )
  => [val]
  -> ReaderT (PersistEntityBackend val) m [Key val]
bulkInsert items = concat <$> forM (chunksOf sqliteMaxVariableNumber items) insertMany

-- | Search for SQLITE_MAX_VARIABLE_NUMBER here:
-- https://www.sqlite.org/limits.html
sqliteMaxVariableNumber :: Int
sqliteMaxVariableNumber = 999


data SqliteEventStore
  = SqliteEventStore
  { _sqliteEventStoreConnectionPool :: ConnectionPool
  }

sqliteEventStore :: (MonadIO m) => ConnectionPool -> m SqliteEventStore
sqliteEventStore pool = do
  -- Run migrations
  liftIO $ runSqlPool (runMigration migrateSqliteEvent) pool

  -- Create index on aggregate_id so retrieval is very fast
  liftIO $ runSqlPool
    (rawExecute "CREATE INDEX IF NOT EXISTS aggregate_id_index ON events (aggregate_id)" [])
    pool

  return $ SqliteEventStore pool

instance (MonadIO m) => RawEventStore m SqliteEventStore ByteString where
  getUuids = sqliteEventStoreGetUuids
  getEvents = sqliteEventStoreGetEvents
  storeEvents = sqliteEventStoreStoreEvents
  latestEventVersion = sqliteEventStoreLatestEventVersion
  getAllEvents = sqliteEventStoreGetAllEvents

instance (FromJSON event, ToJSON event, MonadIO m) => SerializedEventStore m SqliteEventStore ByteString event where
  getSerializedEvents store uuid = do
    rawEvents <- getEvents store uuid
    return $ mapMaybe decodeStoredEvent rawEvents
  getAllSerializedEvents store seqNum = do
    rawEvents <- getAllEvents store seqNum
    return $ mapMaybe decodeStoredEvent rawEvents
  storeSerializedEvents store uuid events = do
    let serialized = toStrict . encode <$> events
    rawStoredEvents <- storeEvents store uuid serialized
    return $ (\(event, StoredEvent uuid' vers seqn _) -> StoredEvent uuid' vers seqn event) <$> zip events rawStoredEvents

instance (MonadIO m, Projection proj, ToJSON (Event proj), FromJSON (Event proj))
         => CachedEventStore m SqliteEventStore ByteString proj where
  -- TODO: Add projection snapshots!

decodeStoredEvent :: (FromJSON event) => StoredEvent ByteString -> Maybe (StoredEvent event)
decodeStoredEvent (StoredEvent uuid' vers seqn bs) = StoredEvent uuid' vers seqn <$> decode (fromStrict bs)

sqliteEventStoreGetUuids :: (MonadIO m) => SqliteEventStore -> m [UUID]
sqliteEventStoreGetUuids (SqliteEventStore pool) =
  liftIO $ runSqlPool getAggregateIds pool

sqliteEventStoreGetEvents :: (MonadIO m) => SqliteEventStore -> UUID -> m [StoredEvent ByteString]
sqliteEventStoreGetEvents (SqliteEventStore pool) uuid =
  liftIO $ runSqlPool (getSqliteAggregateEvents uuid) pool

sqliteEventStoreGetAllEvents :: (MonadIO m) => SqliteEventStore -> SequenceNumber -> m [StoredEvent ByteString]
sqliteEventStoreGetAllEvents (SqliteEventStore pool) seqNum =
  liftIO $ runSqlPool (getAllEventsFromSequence seqNum) pool

sqliteEventStoreStoreEvents :: (MonadIO m) => SqliteEventStore -> UUID -> [ByteString] -> m [StoredEvent ByteString]
sqliteEventStoreStoreEvents (SqliteEventStore pool) uuid events =
  liftIO $ runSqlPool doInsert pool
  where
    doInsert = do
      versionNum <- maxEventVersion uuid
      let eventsAndVers = zip events [versionNum + 1..]
          entities = fmap (uncurry (SqliteEvent uuid)) eventsAndVers
      sequenceNums <- bulkInsert entities
      let eventsAndSeqNums = zip sequenceNums eventsAndVers
      return $ fmap (\(SqliteEventKey seqNum, (event, vers)) -> StoredEvent uuid vers seqNum event) eventsAndSeqNums

sqliteEventStoreLatestEventVersion
  :: (MonadIO m)
  => SqliteEventStore -> UUID -> m EventVersion
sqliteEventStoreLatestEventVersion (SqliteEventStore pool) uuid =
  liftIO $ runSqlPool (maxEventVersion uuid) pool
