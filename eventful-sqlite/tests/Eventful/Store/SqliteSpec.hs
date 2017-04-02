module Eventful.Store.SqliteSpec (spec) where

import Database.Persist.Sqlite
import Test.Hspec

import Eventful.Store.Sqlite
import Eventful.TestHelpers

makeStore :: (MonadIO m) => m (EventStore JSONString (SqlPersistT m), ConnectionPool)
makeStore = do
  pool <- liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1)
  let store = sqliteEventStore defaultSqlEventStoreConfig
  initializeSqliteEventStore defaultSqlEventStoreConfig pool
  return (store, pool)

makeGlobalStore
  :: (MonadIO m)
  => m (EventStore JSONString (SqlPersistT m), GloballyOrderedEventStore JSONString (SqlPersistT m), ConnectionPool)
makeGlobalStore = do
  (store, pool) <- makeStore
  return (store, sqlGloballyOrderedEventStore defaultSqlEventStoreConfig, pool)

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec makeStore (flip runSqlPool) jsonStringSerializer
    sequencedEventStoreSpec makeGlobalStore (flip runSqlPool) jsonStringSerializer
