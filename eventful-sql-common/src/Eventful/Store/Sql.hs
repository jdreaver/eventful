{-# LANGUAGE QuasiQuotes #-}

module Eventful.Store.Sql
  ( module X
  , SqlEvent (..)
  , SqlEventId
  , migrateSqlEvent
  , sqlGetGloballyOrderedEvents
  , sqlGetProjectionIds
  , sqlGetAggregateEvents
  , sqlMaxEventVersion
  , sqlStoreEvents
  ) where

import Eventful.Store.Sql.JSONString as X
import Eventful.Store.Sql.Orphans as X ()

import Control.Monad.Reader
import Data.Maybe (listToMaybe, maybe)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH

import Eventful.Store.Class
import Eventful.UUID

share [mkPersist sqlSettings, mkMigrate "migrateSqlEvent"] [persistLowerCase|
SqlEvent sql=events
    Id SequenceNumber sql=sequence_number
    projectionId UUID
    version EventVersion
    data JSONString
    UniqueAggregateVersion projectionId version
    deriving Show
|]

sqlGetGloballyOrderedEvents
  :: (MonadIO m)
  => GetGloballyOrderedEvents () (StoredEvent JSONString) (SqlPersistT m)
sqlGetGloballyOrderedEvents =
  GetGloballyOrderedEvents $ const sqlGetAllEventsFromSequence

sqlEventToGloballyOrdered :: Entity SqlEvent -> GloballyOrderedEvent (StoredEvent JSONString)
sqlEventToGloballyOrdered (Entity (SqlEventKey seqNum) event) =
  GloballyOrderedEvent seqNum $ sqlEventToStored event

sqlEventToStored :: SqlEvent -> StoredEvent JSONString
sqlEventToStored (SqlEvent uuid version data') =
  StoredEvent uuid version data'

sqlGetProjectionIds :: (MonadIO m) => ReaderT SqlBackend m [UUID]
sqlGetProjectionIds =
  fmap unSingle <$> rawSql "SELECT DISTINCT projection_id FROM events" []

sqlGetAggregateEvents
  :: (MonadIO m)
  => UUID -> Maybe EventVersion -> ReaderT SqlBackend m [StoredEvent JSONString]
sqlGetAggregateEvents uuid mVers = do
  let
    constraints =
      (SqlEventProjectionId ==. uuid) :
      maybe [] (\vers -> [SqlEventVersion >=. vers]) mVers
  entities <- selectList constraints [Asc SqlEventVersion]
  return $ sqlEventToStored . entityVal <$> entities

sqlGetAllEventsFromSequence
  :: (MonadIO m)
  => SequenceNumber -> ReaderT SqlBackend m [GloballyOrderedEvent (StoredEvent JSONString)]
sqlGetAllEventsFromSequence seqNum = do
  entities <- selectList [SqlEventId >=. SqlEventKey seqNum] [Asc SqlEventId]
  return $ sqlEventToGloballyOrdered <$> entities

sqlMaxEventVersion :: (MonadIO m) => Text -> UUID -> ReaderT SqlBackend m EventVersion
sqlMaxEventVersion maxVersionSql uuid =
  let rawVals = rawSql maxVersionSql [toPersistValue uuid]
  in maybe 0 unSingle . listToMaybe <$> rawVals

sqlStoreEvents :: (MonadIO m) => Text -> ([SqlEvent] -> SqlPersistT m ()) -> UUID -> [JSONString] -> SqlPersistT m ()
sqlStoreEvents maxVersionSql bulkInsert uuid events = do
  versionNum <- sqlMaxEventVersion maxVersionSql uuid
  let entities = zipWith (SqlEvent uuid) [versionNum + 1..] events
  bulkInsert entities
