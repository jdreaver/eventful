module Eventful.Store.SqliteSpec (spec) where

import Database.Persist.Sqlite
import Test.Hspec

import Eventful.Serializer
import Eventful.Store.Sqlite
import Eventful.TestHelpers

makeStore :: (MonadIO m) => m (EventStore CounterEvent (SqlPersistT m), ConnectionPool)
makeStore = do
  pool <- liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1)
  let store = serializedEventStore jsonStringSerializer $ sqliteEventStore defaultSqlEventStoreConfig
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  return (store, pool)

makeGlobalStore
  :: (MonadIO m)
  => m (EventStore CounterEvent (SqlPersistT m), GloballyOrderedEventStore CounterEvent (SqlPersistT m), ConnectionPool)
makeGlobalStore = do
  (store, pool) <- makeStore
  let globalStore = serializedGloballyOrderedEventStore jsonStringSerializer (sqlGloballyOrderedEventStore defaultSqlEventStoreConfig)
  return (store, globalStore, pool)

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec makeStore (flip runSqlPool)
    sequencedEventStoreSpec makeGlobalStore (flip runSqlPool)
