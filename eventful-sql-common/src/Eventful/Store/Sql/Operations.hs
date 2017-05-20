module Eventful.Store.Sql.Operations
  ( SqlEventStoreConfig (..)
  , sqlGloballyOrderedEventStore
  , sqlGetProjectionIds
  , sqlGetAggregateEvents
  , sqlMaxEventVersion
  , sqlStoreEvents
  ) where

import Control.Monad.Reader
import Data.Maybe (listToMaybe, maybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql

import Eventful.Store.Class
import Eventful.UUID

import Eventful.Store.Sql.Orphans as X ()

data SqlEventStoreConfig entity serialized =
  SqlEventStoreConfig
  { sqlEventStoreConfigSequenceMakeEntity :: UUID -> EventVersion -> serialized -> entity
    -- Key manipulation
  , sqlEventStoreConfigMakeKey :: SequenceNumber -> Key entity
  , sqlEventStoreConfigUnKey :: Key entity -> SequenceNumber
    -- Record functions
  , sqlEventStoreConfigUUID :: entity -> UUID
  , sqlEventStoreConfigVersion :: entity -> EventVersion
  , sqlEventStoreConfigData :: entity -> serialized
    -- EntityFields
  , sqlEventStoreConfigSequenceNumberField :: EntityField entity (Key entity)
  , sqlEventStoreConfigUUIDField :: EntityField entity UUID
  , sqlEventStoreConfigVersionField :: EntityField entity EventVersion
  , sqlEventStoreConfigDataField :: EntityField entity serialized
  }

sqlGloballyOrderedEventStore
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> GloballyOrderedEventStore serialized (SqlPersistT m)
sqlGloballyOrderedEventStore config =
  GloballyOrderedEventStore $ sqlGetAllEventsFromSequence config

sqlEventToGloballyOrdered
  :: SqlEventStoreConfig entity serialized
  -> Entity entity
  -> GloballyOrderedEvent serialized
sqlEventToGloballyOrdered config@SqlEventStoreConfig{..} (Entity key event) =
  storedEventToGloballyOrderedEvent (sqlEventStoreConfigUnKey key) (sqlEventToStored config event)

sqlEventToStored
  :: SqlEventStoreConfig entity serialized
  -> entity
  -> StoredEvent serialized
sqlEventToStored SqlEventStoreConfig{..} entity =
  StoredEvent
  (sqlEventStoreConfigUUID entity)
  (sqlEventStoreConfigVersion entity)
  (sqlEventStoreConfigData entity)

sqlGetProjectionIds
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> SqlPersistT m [UUID]
sqlGetProjectionIds SqlEventStoreConfig{..} =
  fmap unSingle <$> rawSql ("SELECT DISTINCT " <> uuidFieldName <> " FROM " <> tableName) []
  where
    (DBName tableName) = tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined)
    (DBName uuidFieldName) = fieldDBName sqlEventStoreConfigSequenceNumberField

sqlGetAggregateEvents
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> UUID
  -> Maybe EventVersion
  -> SqlPersistT m [StoredEvent serialized]
sqlGetAggregateEvents config@SqlEventStoreConfig{..} uuid mVers = do
  let
    constraints =
      (sqlEventStoreConfigUUIDField ==. uuid) :
      maybe [] (\vers -> [sqlEventStoreConfigVersionField >=. vers]) mVers
  entities <- selectList constraints [Asc sqlEventStoreConfigVersionField]
  return $ sqlEventToStored config . entityVal <$> entities

sqlGetAllEventsFromSequence
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> SequenceNumber
  -> SqlPersistT m [GloballyOrderedEvent serialized]
sqlGetAllEventsFromSequence config@SqlEventStoreConfig{..} seqNum = do
  entities <-
    selectList
    [sqlEventStoreConfigSequenceNumberField >=. sqlEventStoreConfigMakeKey seqNum]
    [Asc sqlEventStoreConfigSequenceNumberField]
  return $ sqlEventToGloballyOrdered config <$> entities

sqlMaxEventVersion
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> (DBName -> DBName -> DBName -> Text)
  -> UUID
  -> SqlPersistT m EventVersion
sqlMaxEventVersion SqlEventStoreConfig{..} maxVersionSql uuid =
  let
    tableName = tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined)
    uuidFieldName = fieldDBName sqlEventStoreConfigUUIDField
    versionFieldName = fieldDBName sqlEventStoreConfigVersionField
    rawVals = rawSql (maxVersionSql tableName uuidFieldName versionFieldName) [toPersistValue uuid]
  in maybe 0 unSingle . listToMaybe <$> rawVals

sqlStoreEvents
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> (DBName -> DBName -> DBName -> Text)
  -> UUID
  -> [serialized]
  -> SqlPersistT m ()
sqlStoreEvents config@SqlEventStoreConfig{..} maxVersionSql uuid events = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities = zipWith (sqlEventStoreConfigSequenceMakeEntity uuid) [versionNum + 1..] events
  insertMany_ entities
