module Eventful.Store.SqliteSpec (spec) where

import Data.UUID (nil)
import Database.Persist.Sqlite
import Test.Hspec

import Eventful.Store.Sqlite
import Eventful.TestHelpers

makeStore :: (MonadIO m) => m (SqliteEventStore m, ConnectionPool)
makeStore = do
  pool <- liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1)
  initializeSqliteEventStore pool
  return (sqliteEventStore, pool)

spec :: Spec
spec = do
  describe "Sqlite event store" $ do
    eventStoreSpec makeStore (flip runSqlPool)
    sequencedEventStoreSpec makeStore (flip runSqlPool)

    context "when inserting more than SQLITE_MAX_VARIABLE_NUMBER events" $ do
      (store, pool) <- runIO makeStore

      it "doesn't fail with an error about too many variables" $ do
        let numEvents = sqliteMaxVariableNumber * 3
        result <- runSqlPool (runEventStore store $ storeEvents expectNoStream nil (Added <$> [1..numEvents])) pool
        result `shouldBe` Nothing
