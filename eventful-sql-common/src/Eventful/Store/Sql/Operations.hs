{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Eventful.Store.Sql.Operations
  ( SqlEventStoreConfig (..)
  , sqlEventStoreReader
  , sqlGlobalEventStoreReader
  , sqlGetProjectionIds
  , sqlGetAggregateEvents
  , sqlMaxEventVersion
  , sqlStoreEvents
  ) where

import Control.Monad.Reader
import Data.Foldable (for_)
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

sqlEventStoreReader
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> VersionedEventStoreReader (SqlPersistT m) serialized
sqlEventStoreReader config = EventStoreReader $ sqlGetAggregateEvents config

sqlGlobalEventStoreReader
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> GlobalEventStoreReader (SqlPersistT m) serialized
sqlGlobalEventStoreReader config =
  EventStoreReader $ sqlGetAllEventsInRange config

sqlEventToGlobalStream
  :: SqlEventStoreConfig entity serialized
  -> Entity entity
  -> GlobalStreamEvent serialized
sqlEventToGlobalStream config@SqlEventStoreConfig{..} (Entity key event) =
  StreamEvent () (sqlEventStoreConfigUnKey key) (sqlEventToVersioned config event)

sqlEventToVersioned
  :: SqlEventStoreConfig entity serialized
  -> entity
  -> VersionedStreamEvent serialized
sqlEventToVersioned SqlEventStoreConfig{..} entity =
  StreamEvent
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
  -> QueryRange UUID EventVersion
  -> SqlPersistT m [VersionedStreamEvent serialized]
sqlGetAggregateEvents config@SqlEventStoreConfig{..} QueryRange{..} = do
  entities <- selectList filters selectOpts
  return $ sqlEventToVersioned config . entityVal <$> entities
  where
    startFilter =
      case queryRangeStart of
        StartFromBeginning -> []
        StartQueryAt start -> [sqlEventStoreConfigVersionField >=. start]
    (endFilter, endSelectOpt) =
      case queryRangeLimit of
        NoQueryLimit -> ([], [])
        MaxNumberOfEvents maxNum -> ([], [LimitTo maxNum])
        StopQueryAt stop -> ([sqlEventStoreConfigVersionField <=. stop], [])
    filters = (sqlEventStoreConfigUUIDField ==. queryRangeKey) : startFilter ++ endFilter
    selectOpts = Asc sqlEventStoreConfigSequenceNumberField : endSelectOpt

sqlGetAllEventsInRange
  :: (MonadIO m, PersistEntity entity, PersistEntityBackend entity ~ SqlBackend)
  => SqlEventStoreConfig entity serialized
  -> QueryRange () SequenceNumber
  -> SqlPersistT m [GlobalStreamEvent serialized]
sqlGetAllEventsInRange config@SqlEventStoreConfig{..} QueryRange{..} = do
  entities <- selectList filters selectOpts
  return $ sqlEventToGlobalStream config <$> entities
  where
    startFilter =
      case queryRangeStart of
        StartFromBeginning -> []
        StartQueryAt start -> [sqlEventStoreConfigSequenceNumberField >=. sqlEventStoreConfigMakeKey start]
    (endFilter, endSelectOpt) =
      case queryRangeLimit of
        NoQueryLimit -> ([], [])
        MaxNumberOfEvents maxNum -> ([], [LimitTo maxNum])
        StopQueryAt stop -> ([sqlEventStoreConfigSequenceNumberField <=. sqlEventStoreConfigMakeKey stop], [])
    filters = startFilter ++ endFilter
    selectOpts = Asc sqlEventStoreConfigSequenceNumberField : endSelectOpt

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
  -> Maybe (Text -> Text)
  -> (DBName -> DBName -> DBName -> Text)
  -> UUID
  -> [serialized]
  -> SqlPersistT m ()
sqlStoreEvents config@SqlEventStoreConfig{..} mLockCommand maxVersionSql uuid events = do
  versionNum <- sqlMaxEventVersion config maxVersionSql uuid
  let entities = zipWith (sqlEventStoreConfigSequenceMakeEntity uuid) [versionNum + 1..] events
  -- NB: We need to take a lock on the events table or else the global sequence
  -- numbers may not increase monotonically over time.
  for_ mLockCommand $ \lockCommand -> rawExecute (lockCommand tableName) []
  insertMany_ entities
  where
    (DBName tableName) = tableDBName (sqlEventStoreConfigSequenceMakeEntity nil 0 undefined)
