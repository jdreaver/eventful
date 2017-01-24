-- | Common test functionality

module Eventful.TestHelpers
  ( Counter (..)
  , CounterProjection
  , counterProjection
  , CounterAggregate
  , counterAggregate
  , CounterEvent (..)
  , CounterCommand (..)
  , CounterCommandError (..)
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

data CounterEvent
  = Added
    { _counterEventAmount :: Int
    }
  deriving (Eq, Show)

type CounterProjection = Projection Counter CounterEvent

counterProjection :: CounterProjection
counterProjection =
  Projection
  (Counter 0)
  (\(Counter k) (Added x) -> Counter (k + x))

data CounterCommand
  = Increment
    { _counterCommandAmount :: Int
    }
  | Decrement
    { _counterCommandAmount :: Int
    }
  deriving (Eq, Show)

data CounterCommandError
  = OutOfBounds
  deriving (Eq, Show)

type CounterAggregate = Aggregate Counter CounterEvent CounterCommand CounterCommandError

counterAggregate :: CounterAggregate
counterAggregate = Aggregate counterCommand

counterCommand :: Counter -> CounterCommand -> Either CounterCommandError CounterEvent
counterCommand (Counter k) (Increment n) =
  if k + n <= 100
  then Right $ Added n
  else Left OutOfBounds
counterCommand (Counter k) (Decrement n) =
  if k - n >= 0
  then Right $ Added (-n)
  else Left OutOfBounds

deriveJSON (unPrefix "_counterEvent")  ''CounterEvent
deriveJSON (unPrefix "_counterCommand")  ''CounterCommand
deriveJSON (unPrefix "_counterCommandError")  ''CounterCommandError

-- Test harness for stores

eventStoreSpec
  :: (Serializable Counter serialized, Serializable CounterEvent serialized, EventStore m serialized)
  => IO store -> (forall a. store -> m a -> IO a) -> Spec
eventStoreSpec makeStore runStore = do
  context "when the event store is empty" $ do
    store <- runIO makeStore

    it "shouldn't have UUIDs" $ do
      runStore store getAllUuids `shouldReturn` []

    it "should return versions of -1 for UUIDs" $ do
      runStore store (getLatestVersion nil) `shouldReturn` (-1)

  context "when a few events are inserted" $ do
    let events = [Added 1, Added 4, Added (-3)]
    store <- runIO makeStore
    _ <- runIO $ runStore store $ storeEvents nil events

    it "should return events" $ do
      events' <- runStore store $ getEvents nil
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return correct event versions" $ do
      runStore store (getLatestVersion nil) `shouldReturn` 2
      fmap storedEventEvent <$> runStore store (getEventsFromVersion nil (-1))
        >>= (`shouldBe` events)
      fmap storedEventEvent <$> runStore store (getEventsFromVersion nil 1)
        >>= (`shouldBe` drop 1 events)

    it "should return the latest projection" $ do
      runStore store (getLatestProjection counterProjection nil) `shouldReturn` Counter 2

  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO makeStore
    (uuid1, uuid2) <- runIO $ runStore store insertExampleEvents

    it "should have the correct events for each aggregate" $ do
      events1 <- runStore store $ getEvents uuid1
      events2 <- runStore store $ getEvents uuid2
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventProjectionId <$> events1) `shouldBe` [uuid1, uuid1]
      (storedEventProjectionId <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      runStore store (getLatestVersion uuid1) `shouldReturn` 1
      runStore store (getLatestVersion uuid2) `shouldReturn` 2
      fmap storedEventEvent <$> runStore store (getEventsFromVersion uuid1 0) >>= (`shouldBe` [Added 1, Added 4])
      fmap storedEventEvent <$> runStore store (getEventsFromVersion uuid2 1) >>= (`shouldBe` [Added 3, Added 5])

    it "should produce the correct projections" $ do
      runStore store (getLatestProjection counterProjection uuid1) `shouldReturn` Counter 5
      runStore store (getLatestProjection counterProjection uuid2) `shouldReturn` Counter 10


sequencedEventStoreSpec
  :: (Serializable CounterEvent serialized, EventStore m serialized)
  => IO store -> (forall a. store -> m a -> IO a) -> Spec
sequencedEventStoreSpec makeStore runStore = do
  context "when the event store is empty" $ do
    store <- runIO makeStore

    it "shouldn't have any events" $ do
      length <$> runStore store (getSequencedEvents 0) `shouldReturn` 0

  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO makeStore
    (uuid1, uuid2) <- runIO $ runStore store insertExampleEvents

    it "should have the correct events in global order" $ do
      events' <- runStore store $ getSequencedEvents 0
      let deserializedEvents = mapMaybe deserialize events'
      (storedEventEvent <$> deserializedEvents) `shouldBe` Added <$> [1..5]
      (storedEventProjectionId <$> deserializedEvents) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (storedEventVersion <$> deserializedEvents) `shouldBe` [0, 0, 1, 1, 2]
      (storedEventSequenceNumber <$> deserializedEvents) `shouldBe` [1..5]

insertExampleEvents
  :: (Serializable CounterEvent serialized, EventStore m serialized)
  => m (UUID, UUID)
insertExampleEvents = do
  let uuid1 = uuidFromInteger 1
      uuid2 = uuidFromInteger 2
  void $ storeEvents uuid1 [Added 1]
  void $ storeEvents uuid2 [Added 2, Added 3]
  void $ storeEvents uuid1 [Added 4]
  void $ storeEvents uuid2 [Added 5]
  return (uuid1, uuid2)
