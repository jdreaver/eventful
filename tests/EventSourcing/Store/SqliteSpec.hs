module EventSourcing.Store.SqliteSpec (spec) where

import TestImport

import Test.Hspec

import EventSourcing.Store.Class
import EventSourcing.Store.Sqlite
import EventSourcing.UUID

-- withPool :: (MonadIO m) => (ConnectionPool -> m a) -> m a
-- withPool f = do
--   pool <- liftIO $ runNoLoggingT $ createSqlitePool ":memory:" 1
--   f pool

-- withStore :: (MonadIO m) => (SqliteEventStore -> m a) -> m a
-- withStore f = withPool $ \pool -> do
--   store <- liftIO (sqliteEventStore pool)
--   f store

makeStore :: (MonadIO m) => m SqliteEventStore
makeStore = liftIO $ runNoLoggingT (createSqlitePool ":memory:" 1) >>= sqliteEventStore

spec :: Spec
spec = do
  context "when the sqlite store is empty" $ do
    store <- runIO makeStore

    it "shouldn't have UUIDs" $ do
      getUuids store `shouldReturn` []

    it "shouldn't have any events" $ do
      getAllEvents store 0 `shouldReturn` []

  context "when a few events are inserted" $ do
    store <- runIO makeStore
    let events = [Added 1, Added 4, Added (-3)]
    _ <- runIO $ storeSerializedEvents store nil events

    it "should return events" $ do
      events' <- getSerializedEvents store nil
      (storedEventEvent <$> events') `shouldBe` events
      (storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return the latest projection" $ do
      getAggregateFromSerialized store (AggregateId nil) `shouldReturn` Counter 2

  context "when events from multiple UUIDs are inserted" $ do
    let uuid1 = uuidFromInteger 1
        uuid2 = uuidFromInteger 2
    store <- runIO makeStore
    _ <- runIO $ do
      void $ storeSerializedEvents store uuid1 [Added 1]
      void $ storeSerializedEvents store uuid2 [Added 2, Added 3]
      void $ storeSerializedEvents store uuid1 [Added 4]
      void $ storeSerializedEvents store uuid2 [Added 5]

    it "should have the correct events in order global" $ do
      events' <- getAllSerializedEvents store 0
      (storedEventEvent <$> events') `shouldBe` Added <$> [1..5]
      (storedEventAggregateId <$> events') `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (storedEventVersion <$> events') `shouldBe` [0, 0, 1, 1, 2]
      (storedEventSequenceNumber <$> events') `shouldBe` [1..5]

    it "should produce the correct projections" $ do
      getAggregateFromSerialized store (AggregateId uuid1) `shouldReturn` Counter 5
      getAggregateFromSerialized store (AggregateId uuid2) `shouldReturn` Counter 10
