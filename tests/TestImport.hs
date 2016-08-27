-- | Common test functionality

module TestImport
  ( module X
  , Counter (..)
  , Event (..)
  , Command (..)
  , CommandError (..)
  , rawStoreSpec
  , sequencedRawStoreSpec
  , serializedStoreSpec
  , sequencedSerializedStoreSpec
  ) where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Logger as X
import Database.Persist.Sqlite as X

import Data.Aeson
import Data.Aeson.TH
import Test.Hspec

import EventSourcing

-- | Example Projection/Aggregate
newtype Counter = Counter { unCounter :: Int }
  deriving (Eq, Show, FromJSON, ToJSON)

instance Projection Counter where
  data Event Counter
    = Added
    { _counterEventAmount :: Int
    }
    deriving (Eq, Show)
  seed = Counter 0
  apply (Counter k) (Added x) = Counter (k + x)

instance Aggregate Counter where
  data Command Counter
    = Increment
      { _counterCommandAmount :: Int
      }
    | Decrement
      { _counterCommandAmount :: Int
      }
    deriving (Eq, Show)

  data CommandError Counter
    = OutOfBounds
    deriving (Eq, Show)

  command (Counter k) (Increment n) =
    if k + n <= 100
    then Right $ Added n
    else Left OutOfBounds

  command (Counter k) (Decrement n) =
    if k - n >= 0
    then Right $ Added (-n)
    else Left OutOfBounds

deriveJSON defaultOptions 'Added
deriveJSON defaultOptions 'Increment
deriveJSON defaultOptions 'OutOfBounds


-- Test harness for stores

rawStoreSpec :: (RawEventStore IO store serialized) => IO store -> Spec
rawStoreSpec createStore = do
  context "when the event store is empty" $ do
    store <- runIO createStore

    it "shouldn't have UUIDs" $ do
      getUuids store `shouldReturn` []

sequencedRawStoreSpec :: (Show serialized, Eq serialized, SequencedRawEventStore IO store serialized) => IO store -> Spec
sequencedRawStoreSpec createStore = do
  context "when the event store is empty" $ do
    store <- runIO createStore

    it "shouldn't have any events" $ do
      getSequencedEvents store 0 `shouldReturn` []

serializedStoreSpec :: (SerializedEventStore IO store serialized (Event Counter)) => IO store -> Spec
serializedStoreSpec createStore = do
  context "when a few events are inserted" $ do
    store <- runIO createStore
    let events = [Added 1, Added 4, Added (-3)]
    _ <- runIO $ storeSerializedEvents store nil events

    it "should return events" $ do
      events' <- getSerializedEvents store nil
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return the latest projection" $ do
      getAggregateFromSerialized store (AggregateId nil) `shouldReturn` Counter 2

  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO createStore
    (uuid1, uuid2) <- runIO $ insertExampleEvents store

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

sequencedSerializedStoreSpec :: (SequencedSerializedEventStore IO store serialized (Event Counter)) => IO store -> Spec
sequencedSerializedStoreSpec createStore = do
  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO createStore
    (uuid1, uuid2) <- runIO $ insertExampleEvents store

    it "should have the correct events in global order" $ do
      events' <- getSequencedSerializedEvents store 0
      (sequencedEventEvent <$> events') `shouldBe` Added <$> [1..5]
      (sequencedEventAggregateId <$> events') `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (sequencedEventVersion <$> events') `shouldBe` [0, 0, 1, 1, 2]
      (sequencedEventSequenceNumber <$> events') `shouldBe` [1..5]

insertExampleEvents :: (SerializedEventStore IO store serialized (Event Counter)) => store -> IO (UUID, UUID)
insertExampleEvents store = do
  let uuid1 = uuidFromInteger 1
      uuid2 = uuidFromInteger 2
  void $ storeSerializedEvents store uuid1 [Added 1]
  void $ storeSerializedEvents store uuid2 [Added 2, Added 3]
  void $ storeSerializedEvents store uuid1 [Added 4]
  void $ storeSerializedEvents store uuid2 [Added 5]
  return (uuid1, uuid2)
