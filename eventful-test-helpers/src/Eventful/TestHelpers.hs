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
import Data.Aeson.Casing
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
counterAggregate = Aggregate counterCommand counterProjection

counterCommand :: Counter -> CounterCommand -> Either CounterCommandError [CounterEvent]
counterCommand (Counter k) (Increment n) =
  if k + n <= 100
  then Right [Added n]
  else Left OutOfBounds
counterCommand (Counter k) (Decrement n) =
  if k - n >= 0
  then Right [Added (-n)]
  else Left OutOfBounds

deriveJSON (aesonPrefix camelCase) ''CounterEvent
deriveJSON (aesonPrefix camelCase) ''CounterCommand
deriveJSON (aesonPrefix camelCase) ''CounterCommandError

-- Test harness for stores

eventStoreSpec
  :: (Serializable Counter serialized, Serializable CounterEvent serialized, Monad m)
  => IO (EventStore store serialized m, runargs) -> (forall a. runargs -> m a -> IO a) -> Spec
eventStoreSpec makeStore runAsIO = do
  context "when the event store is empty" $ do

    it "shouldn't have UUIDs" $ do
      -- TODO: Abstract out creating all of these stores and having to manually
      -- thread around makeStore, runAsIO, etc.
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (runEventStore store getAllUuids) `shouldReturn` []

    it "should return versions of -1 for UUIDs" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (runEventStore store (getLatestVersion nil)) `shouldReturn` (-1)

  context "when a few events are inserted" $ do
    let
      events = [Added 1, Added 4, Added (-3)]
      buildStore = do
        (store, runargs) <- liftIO makeStore
        _ <- liftIO . runAsIO runargs . runEventStore store $ storeEvents NoStream nil events
        return (store, runargs)

    it "should return events" $ do
      (store, runargs) <- liftIO buildStore
      events' <- runAsIO runargs $ runEventStore store $ getEvents nil
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return correct event versions" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (runEventStore store (getLatestVersion nil)) `shouldReturn` 2
      runAsIO runargs (fmap storedEventEvent <$> runEventStore store (getEventsFromVersion nil (-1)))
        >>= (`shouldBe` events)
      runAsIO runargs (fmap storedEventEvent <$> runEventStore store (getEventsFromVersion nil 1))
        >>= (`shouldBe` drop 1 events)

    it "should return the latest projection" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (runEventStore store (getLatestProjection counterProjection nil))
        `shouldReturn` (Counter 2, 2)

  context "when events from multiple UUIDs are inserted" $ do
    let
      buildStore = do
        (store, runargs) <- liftIO makeStore
        _ <- liftIO . runAsIO runargs . runEventStore store $ insertExampleEvents
        return (store, runargs)

    it "should have the correct events for each aggregate" $ do
      (store, runargs) <- liftIO buildStore
      events1 <- runAsIO runargs . runEventStore store $ getEvents uuid1
      events2 <- runAsIO runargs . runEventStore store $ getEvents uuid2
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventProjectionId <$> events1) `shouldBe` [uuid1, uuid1]
      (storedEventProjectionId <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (runEventStore store $ getLatestVersion uuid1) `shouldReturn` 1
      runAsIO runargs (runEventStore store $ getLatestVersion uuid2) `shouldReturn` 2
      fmap storedEventEvent <$> runAsIO runargs (runEventStore store $ getEventsFromVersion uuid1 0)
        >>= (`shouldBe` [Added 1, Added 4])
      fmap storedEventEvent <$> runAsIO runargs (runEventStore store $ getEventsFromVersion uuid2 1)
        >>= (`shouldBe` [Added 3, Added 5])

    it "should produce the correct projections" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (runEventStore store $ getLatestProjection counterProjection uuid1)
        `shouldReturn` (Counter 5, 1)
      runAsIO runargs (runEventStore store $ getLatestProjection counterProjection uuid2)
        `shouldReturn` (Counter 10, 2)

  describe "can handle event storage errors" $ do

    it "rejects some writes when event store isn't created" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (runEventStore store $ storeEvents StreamExists nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion (-1))
      runAsIO runargs (runEventStore store $ storeEvents (ExactVersion 0) nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion (-1))

    it "should be able to store events starting with an empty stream" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (runEventStore store $ storeEvents NoStream nil [Added 1]) `shouldReturn` Nothing

    it "should reject storing events sometimes with a stream" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (runEventStore store $ storeEvents NoStream nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (runEventStore store $ storeEvents NoStream nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion 0)
      runAsIO runargs (runEventStore store $ storeEvents (ExactVersion 1) nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion 0)

    it "should accepts storing events sometimes with a stream" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (runEventStore store $ storeEvents NoStream nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (runEventStore store $ storeEvents AnyVersion nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (runEventStore store $ storeEvents (ExactVersion 1) nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (runEventStore store $ storeEvents StreamExists nil [Added 1]) `shouldReturn` Nothing

sequencedEventStoreSpec
  :: (Serializable CounterEvent serialized, Monad m)
  => GetGloballyOrderedEvents store (StoredEvent serialized) m
  -> IO (EventStore store serialized m, runargs)
  -> (forall a. runargs -> m a -> IO a)
  -> Spec
sequencedEventStoreSpec globalDef makeStore runAsIO = do
  context "when the event store is empty" $ do

    it "shouldn't have any events" $ do
      (store, runargs) <- liftIO makeStore
      length <$> runAsIO runargs (runEventStore store (getSequencedEvents globalDef 0)) `shouldReturn` 0

  context "when events from multiple UUIDs are inserted" $ do
    let
      buildStore = do
        (store, runargs) <- liftIO makeStore
        _ <- liftIO . runAsIO runargs . runEventStore store $ insertExampleEvents
        return (store, runargs)

    it "should have the correct events in global order" $ do
      (store, runargs) <- liftIO buildStore
      events' <- runAsIO runargs . runEventStore store $ getSequencedEvents globalDef 0
      let deserializedEvents = mapMaybe deserialize events'
      (storedEventEvent . globallyOrderedEventEvent <$> deserializedEvents) `shouldBe` Added <$> [1..5]
      (storedEventProjectionId . globallyOrderedEventEvent <$> deserializedEvents) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (storedEventVersion . globallyOrderedEventEvent <$> deserializedEvents) `shouldBe` [0, 0, 1, 1, 2]
      (globallyOrderedEventSequenceNumber <$> deserializedEvents) `shouldBe` [1..5]

insertExampleEvents
  :: (Serializable CounterEvent serialized, Monad m)
  => EventStoreT store serialized m ()
insertExampleEvents = do
  void $ storeEvents NoStream uuid1 [Added 1]
  void $ storeEvents NoStream uuid2 [Added 2, Added 3]
  void $ storeEvents (ExactVersion 0) uuid1 [Added 4]
  void $ storeEvents (ExactVersion 1) uuid2 [Added 5]

uuid1 :: UUID
uuid1 = uuidFromInteger 1

uuid2 :: UUID
uuid2 = uuidFromInteger 2
