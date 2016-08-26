module EventSourcing.Store.MemorySpec (spec) where

import TestImport

import Test.Hspec

import EventSourcing.Store.Class
import EventSourcing.Store.Memory
import EventSourcing.UUID

spec :: Spec
spec = do
  context "when the TVar memory event store is empty" $ do
    store <- runIO eventStoreMapTVar

    it "shouldn't have UUIDs" $ do
      getUuids store `shouldReturn` []

  context "when a few events are inserted" $ do
    store <- runIO eventStoreMapTVar
    let events = [Added 1, Added 4, Added (-3)]
    _ <- runIO $ storeSerializedEvents store nil events

    it "should return events" $ do
      events' <- getSerializedEvents store nil
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return the latest projection" $ do
      getAggregateFromSerialized store (AggregateId nil) `shouldReturn` Counter 2

  context "when events from multiple UUIDs are inserted" $ do
    let uuid1 = uuidFromInteger 1
        uuid2 = uuidFromInteger 2
    store <- runIO eventStoreMapTVar
    _ <- runIO $ do
      void $ storeSerializedEvents store uuid1 [Added 1]
      void $ storeSerializedEvents store uuid2 [Added 2, Added 3]
      void $ storeSerializedEvents store uuid1 [Added 4]
      void $ storeSerializedEvents store uuid2 [Added 5]

    it "should have the correct events for each aggregate" $ do
      events1 <- getSerializedEvents store uuid1
      events2 <- getSerializedEvents store uuid2
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventAggregateId <$> events1) `shouldBe` [uuid1, uuid1]
      (storedEventAggregateId <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should produce the correct projections" $ do
      getAggregateFromSerialized store (AggregateId uuid1) `shouldReturn` Counter 5
      getAggregateFromSerialized store (AggregateId uuid2) `shouldReturn` Counter 10
