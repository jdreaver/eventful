{-# LANGUAGE OverloadedStrings #-}

module Eventful.Store.SqliteSpec (spec) where

import Database.Persist.Sqlite
import Test.Hspec

import Eventful.Store.Sqlite
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec sqliteStoreRunner
    globalStreamEventStoreSpec sqliteStoreGlobalRunner

  -- This is really a test for runEventStoreUsing and
  -- runGlobalStreamEventStoreUsing. This is just a good place to put it.
  describe "runEventStoreUsing and runGlobalStreamEventStoreUsing for the sqlite store" $ do
    eventStoreSpec sqliteIOStoreRunner
    globalStreamEventStoreSpec sqliteIOStoreGlobalRunner

makeStore :: IO (VersionedEventStoreWriter (SqlPersistT IO) CounterEvent, VersionedEventStoreReader (SqlPersistT IO) CounterEvent, ConnectionPool)
makeStore = do
  pool <- liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1)
  let
    writer = serializedEventStoreWriter jsonStringSerializer $ sqliteEventStoreWriter defaultSqlEventStoreConfig
    reader = serializedVersionedEventStoreReader jsonStringSerializer $ sqlEventStoreReader defaultSqlEventStoreConfig
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  return (writer, reader, pool)

sqliteStoreRunner :: EventStoreRunner (SqlPersistT IO)
sqliteStoreRunner = EventStoreRunner $ \action -> do
  (writer, reader, pool) <- makeStore
  runSqlPool (action writer reader) pool

sqliteStoreGlobalRunner :: GlobalStreamEventStoreRunner (SqlPersistT IO)
sqliteStoreGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  (writer, _, pool) <- makeStore
  let globalStore = serializedGlobalEventStoreReader jsonStringSerializer (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)
  runSqlPool (action writer globalStore) pool

makeIOStore :: IO (VersionedEventStoreWriter IO CounterEvent, VersionedEventStoreReader IO CounterEvent, ConnectionPool)
makeIOStore = do
  (writer, reader, pool) <- makeStore
  let
    writer' = runEventStoreWriterUsing (flip runSqlPool pool) writer
    reader' = runEventStoreReaderUsing (flip runSqlPool pool) reader
  return (writer', reader', pool)

sqliteIOStoreRunner :: EventStoreRunner IO
sqliteIOStoreRunner = EventStoreRunner $ \action -> do
  (writer, reader, _) <- makeIOStore
  action writer reader

sqliteIOStoreGlobalRunner :: GlobalStreamEventStoreRunner IO
sqliteIOStoreGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  (writer, _, pool) <- makeIOStore
  let
    globalStore = serializedGlobalEventStoreReader jsonStringSerializer (sqlGlobalEventStoreReader defaultSqlEventStoreConfig)
    globalStore' = runEventStoreReaderUsing (flip runSqlPool pool) globalStore
  action writer globalStore'
