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

makeStore :: IO (EventStore CounterEvent (SqlPersistT IO), ConnectionPool)
makeStore = do
  pool <- liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1)
  let store = serializedEventStore jsonStringSerializer $ sqliteEventStore defaultSqlEventStoreConfig
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  return (store, pool)

sqliteStoreRunner :: EventStoreRunner (SqlPersistT IO)
sqliteStoreRunner = EventStoreRunner $ \action -> do
  (store, pool) <- makeStore
  runSqlPool (action store) pool

sqliteStoreGlobalRunner :: GlobalStreamEventStoreRunner (SqlPersistT IO)
sqliteStoreGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  (store, pool) <- makeStore
  let globalStore = serializedGlobalStreamEventStore jsonStringSerializer (sqlGlobalStreamEventStore defaultSqlEventStoreConfig)
  runSqlPool (action store globalStore) pool

makeIOStore :: IO (EventStore CounterEvent IO, ConnectionPool)
makeIOStore = do
  (store, pool) <- makeStore
  let store' = runEventStoreUsing (flip runSqlPool pool) store
  return (store', pool)

sqliteIOStoreRunner :: EventStoreRunner IO
sqliteIOStoreRunner = EventStoreRunner $ \action -> do
  (store, _) <- makeIOStore
  action store

sqliteIOStoreGlobalRunner :: GlobalStreamEventStoreRunner IO
sqliteIOStoreGlobalRunner = GlobalStreamEventStoreRunner $ \action -> do
  (store, pool) <- makeIOStore
  let
    globalStore = serializedGlobalStreamEventStore jsonStringSerializer (sqlGlobalStreamEventStore defaultSqlEventStoreConfig)
    globalStore' = runGlobalStreamEventStoreUsing (flip runSqlPool pool) globalStore
  action store globalStore'
