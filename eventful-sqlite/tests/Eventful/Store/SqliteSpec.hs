module Eventful.Store.SqliteSpec (spec) where

import Data.UUID (nil)
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

    context "when inserting more than SQLITE_MAX_VARIABLE_NUMBER events" $ do
      store <- runIO makeStore

      it "doesn't fail with an error about too many variables" $ do
        let numEvents = sqliteMaxVariableNumber * 3
        storedEvents <- storeEvents store nil (Added <$> [1..numEvents])
        length storedEvents `shouldBe` numEvents
