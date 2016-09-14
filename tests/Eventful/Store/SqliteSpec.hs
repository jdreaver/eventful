module Eventful.Store.SqliteSpec (spec) where

import TestImport

import Test.Hspec

import Eventful.Store.Sqlite

makeStore :: (MonadIO m) => m SqliteEventStore
makeStore = liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1) >>= sqliteEventStore

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec makeStore
    sequencedEventStoreSpec makeStore
