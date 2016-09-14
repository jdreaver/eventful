module Eventful.Store.SqliteSpec (spec) where

import Database.Persist.Sqlite
import Test.Hspec

import Eventful.Store.Sqlite
import Eventful.TestHelpers

makeStore :: (MonadIO m) => m SqliteEventStore
makeStore = liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1) >>= sqliteEventStore

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec makeStore
    sequencedEventStoreSpec makeStore
