module EventSourcing.Store.SqliteSpec (spec) where

import TestImport

import Test.Hspec

import EventSourcing.Store.Sqlite

makeStore :: (MonadIO m) => m SqliteEventStore
makeStore = liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1) >>= sqliteEventStore

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    rawStoreSpec makeStore
    sequencedRawStoreSpec makeStore
    serializedStoreSpec makeStore
    sequencedSerializedStoreSpec makeStore
