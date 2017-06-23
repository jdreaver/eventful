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
    sequencedEventStoreSpec sqliteStoreGlobalRunner

  -- This is really a test for runEventStoreUsing and
  -- runGloballyOrderedEventStoreUsing. This is just a good place to put it.
  describe "runEventStoreUsing and runGloballyOrderedEventStoreUsing for the sqlite store" $ do
    eventStoreSpec sqliteIOStoreRunner
    sequencedEventStoreSpec sqliteIOStoreGlobalRunner

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

sqliteStoreGlobalRunner :: GloballyOrderedEventStoreRunner (SqlPersistT IO)
sqliteStoreGlobalRunner = GloballyOrderedEventStoreRunner $ \action -> do
  (store, pool) <- makeStore
  let globalStore = serializedGloballyOrderedEventStore jsonStringSerializer (sqlGloballyOrderedEventStore defaultSqlEventStoreConfig)
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

sqliteIOStoreGlobalRunner :: GloballyOrderedEventStoreRunner IO
sqliteIOStoreGlobalRunner = GloballyOrderedEventStoreRunner $ \action -> do
  (store, pool) <- makeIOStore
  let
    globalStore = serializedGloballyOrderedEventStore jsonStringSerializer (sqlGloballyOrderedEventStore defaultSqlEventStoreConfig)
    globalStore' = runGloballyOrderedEventStoreUsing (flip runSqlPool pool) globalStore
  action store globalStore'
