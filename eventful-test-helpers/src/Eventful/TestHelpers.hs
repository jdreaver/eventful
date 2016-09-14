-- | Common test functionality

module Eventful.TestHelpers
  ( Counter (..)
  , Event (..)
  , Command (..)
  , CommandError (..)
  , eventStoreSpec
  , sequencedEventStoreSpec
  , module X
  ) where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Logger as X

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Test.Hspec

import Eventful

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

deriveJSON (unPrefix "_counterEvent") 'Added
deriveJSON (unPrefix "_counterCommand") 'Increment
deriveJSON (unPrefix "_counterCommandError") 'OutOfBounds


-- Test harness for stores

eventStoreSpec
  :: (Serializable (Event Counter) serialized, EventStore IO store serialized)
  => IO store -> Spec
eventStoreSpec createStore = do
  context "when the event store is empty" $ do
    store <- runIO createStore

    it "shouldn't have UUIDs" $ do
      getAllUuids store `shouldReturn` []

  context "when a few events are inserted" $ do
    store <- runIO createStore
    let events = [Added 1, Added 4, Added (-3)]
    _ <- runIO $ storeEvents store (AggregateId nil) events

    it "should return events" $ do
      events' <- getEvents store (AggregateId nil)
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return the latest projection" $ do
      getAggregate store (AggregateId nil) `shouldReturn` Counter 2

  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO createStore
    (uuid1, uuid2) <- runIO $ insertExampleEvents store
    let (AggregateId uuid1', AggregateId uuid2') = (uuid1, uuid2)

    it "should have the correct events for each aggregate" $ do
      events1 <- getEvents store uuid1
      events2 <- getEvents store uuid2
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventAggregateId <$> events1) `shouldBe` [uuid1', uuid1']
      (storedEventAggregateId <$> events2) `shouldBe` [uuid2', uuid2', uuid2']
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should produce the correct projections" $ do
      getAggregate store uuid1 `shouldReturn` Counter 5
      getAggregate store uuid2 `shouldReturn` Counter 10


sequencedEventStoreSpec
  :: (Serializable (Event Counter) serialized, EventStore IO store serialized)
  => IO store -> Spec
sequencedEventStoreSpec createStore = do
  context "when the event store is empty" $ do
    store <- runIO createStore

    it "shouldn't have any events" $ do
      length <$> getSequencedEvents store 0 `shouldReturn` 0

  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO createStore
    (AggregateId uuid1, AggregateId uuid2) <- runIO $ insertExampleEvents store

    it "should have the correct events in global order" $ do
      events' <- getSequencedEvents store 0
      let deserializedEvents = mapMaybe deserializeEvent events'
      (storedEventEvent <$> deserializedEvents) `shouldBe` Added <$> [1..5]
      (storedEventAggregateId <$> deserializedEvents) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (storedEventVersion <$> deserializedEvents) `shouldBe` [0, 0, 1, 1, 2]
      (storedEventSequenceNumber <$> deserializedEvents) `shouldBe` [1..5]

insertExampleEvents
  :: (Serializable (Event Counter) serialized, EventStore IO store serialized)
  => store -> IO (AggregateId Counter, AggregateId Counter)
insertExampleEvents store = do
  let uuid1 = AggregateId (uuidFromInteger 1)
      uuid2 = AggregateId (uuidFromInteger 2)
  void $ storeEvents store uuid1 [Added 1]
  void $ storeEvents store uuid2 [Added 2, Added 3]
  void $ storeEvents store uuid1 [Added 4]
  void $ storeEvents store uuid2 [Added 5]
  return (uuid1, uuid2)
