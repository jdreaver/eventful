{-# LANGUAGE OverloadedStrings #-}

module Eventful.Store.SqliteSpec (spec) where

import Database.Persist.Sqlite
import Test.Hspec

import Eventful.Serializer
import Eventful.Store.Sqlite
import Eventful.TestHelpers

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec sqliteStoreRunner
    sequencedEventStoreSpec sqliteStoreGlobalRunner

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
