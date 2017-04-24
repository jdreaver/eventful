-- | Common test functionality

module Eventful.TestHelpers
  ( Counter (..)
  , CounterProjection
  , counterProjection
  , CounterAggregate
  , counterAggregate
  , CounterEvent (..)
  , CounterCommand (..)
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
import Test.Hspec

import Eventful

-- | Example Projection/Aggregate
newtype Counter = Counter { unCounter :: Int }
  deriving (Eq, Show, FromJSON, ToJSON)

data CounterEvent
  = Added
    { _counterEventAmount :: Int
    }
  | CounterFailedOutOfBounds
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

type CounterAggregate = Aggregate Counter CounterEvent CounterCommand

counterAggregate :: CounterAggregate
counterAggregate = Aggregate counterCommand counterProjection

counterCommand :: Counter -> CounterCommand -> [CounterEvent]
counterCommand (Counter k) (Increment n) =
  if k + n <= 100
  then [Added n]
  else [CounterFailedOutOfBounds]
counterCommand (Counter k) (Decrement n) =
  if k - n >= 0
  then [Added (-n)]
  else [CounterFailedOutOfBounds]

deriveJSON (aesonPrefix camelCase) ''CounterEvent
deriveJSON (aesonPrefix camelCase) ''CounterCommand

-- Test harness for stores

eventStoreSpec
  :: (Monad m)
  => IO (EventStore CounterEvent m, runargs)
  -> (forall a. runargs -> m a -> IO a)
  -> Spec
eventStoreSpec makeStore runAsIO = do
  context "when the event store is empty" $ do

    it "should return versions of -1 for a UUID" $ do
      -- TODO: Abstract out creating all of these stores and having to manually
      -- thread around makeStore, runAsIO, etc.
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (getLatestVersion store nil) `shouldReturn` (-1)

  context "when a few events are inserted" $ do
    let
      events = [Added 1, Added 4, Added (-3)]
      buildStore = do
        (store, runargs) <- liftIO makeStore
        _ <- liftIO . runAsIO runargs $ storeEvents store NoStream nil events
        return (store, runargs)

    it "should return events" $ do
      (store, runargs) <- liftIO buildStore
      events' <- runAsIO runargs (getEvents store nil Nothing)
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return correct event versions" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (getLatestVersion store nil) `shouldReturn` 2
      runAsIO runargs (fmap storedEventEvent <$> getEvents store nil (Just (-1)))
        >>= (`shouldBe` events)
      runAsIO runargs (fmap storedEventEvent <$> getEvents store nil (Just 1))
        >>= (`shouldBe` drop 1 events)

    it "should return the latest projection" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (getLatestProjection store counterProjection nil)
        `shouldReturn` (Counter 2, 2)

  context "when events from multiple UUIDs are inserted" $ do
    let
      buildStore = do
        (store, runargs) <- liftIO makeStore
        _ <- liftIO . runAsIO runargs $ insertExampleEvents store
        return (store, runargs)

    it "should have the correct events for each aggregate" $ do
      (store, runargs) <- liftIO buildStore
      events1 <- runAsIO runargs (getEvents store uuid1 Nothing)
      events2 <- runAsIO runargs (getEvents store uuid2 Nothing)
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventProjectionId <$> events1) `shouldBe` [uuid1, uuid1]
      (storedEventProjectionId <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (getLatestVersion store uuid1) `shouldReturn` 1
      runAsIO runargs (getLatestVersion store uuid2) `shouldReturn` 2
      events1 <- runAsIO runargs (getEvents store uuid1 (Just 0))
      events2 <- runAsIO runargs (getEvents store uuid2 (Just 1))
      storedEventEvent <$> events1 `shouldBe` [Added 1, Added 4]
      storedEventEvent <$> events2 `shouldBe` [Added 3, Added 5]

    it "should produce the correct projections" $ do
      (store, runargs) <- liftIO buildStore
      runAsIO runargs (getLatestProjection store counterProjection uuid1)
        `shouldReturn` (Counter 5, 1)
      runAsIO runargs (getLatestProjection store counterProjection uuid2)
        `shouldReturn` (Counter 10, 2)

  describe "can handle event storage errors" $ do

    it "rejects some writes when event store isn't created" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (storeEvents store StreamExists nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion (-1))
      runAsIO runargs (storeEvents store (ExactVersion 0) nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion (-1))

    it "should be able to store events starting with an empty stream" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (storeEvents store NoStream nil [Added 1]) `shouldReturn` Nothing

    it "should reject storing events sometimes with a stream" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (storeEvents store NoStream nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (storeEvents store NoStream nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion 0)
      runAsIO runargs (storeEvents store (ExactVersion 1) nil [Added 1])
        `shouldReturn` Just (EventStreamNotAtExpectedVersion 0)

    it "should accepts storing events sometimes with a stream" $ do
      (store, runargs) <- liftIO makeStore
      runAsIO runargs (storeEvents store NoStream nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (storeEvents store AnyVersion nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (storeEvents store (ExactVersion 1) nil [Added 1]) `shouldReturn` Nothing
      runAsIO runargs (storeEvents store StreamExists nil [Added 1]) `shouldReturn` Nothing

sequencedEventStoreSpec
  :: (Monad m)
  => IO (EventStore CounterEvent m, GloballyOrderedEventStore CounterEvent m, runargs)
  -> (forall a. runargs -> m a -> IO a)
  -> Spec
sequencedEventStoreSpec makeStore runAsIO = do
  context "when the event store is empty" $ do

    it "shouldn't have any events" $ do
      (_, globalStore, runargs) <- liftIO makeStore
      length <$> runAsIO runargs (getSequencedEvents globalStore 0) `shouldReturn` 0

  context "when events from multiple UUIDs are inserted" $ do
    let
      buildStore = do
        (store, globalStore, runargs) <- liftIO makeStore
        _ <- liftIO . runAsIO runargs $ insertExampleEvents store
        return (globalStore, runargs)

    it "should have the correct events in global order" $ do
      (store, runargs) <- liftIO buildStore
      events' <- runAsIO runargs $ getSequencedEvents store 0
      (storedEventEvent . globallyOrderedEventEvent <$> events') `shouldBe` Added <$> [1..5]
      (storedEventProjectionId . globallyOrderedEventEvent <$> events') `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (storedEventVersion . globallyOrderedEventEvent <$> events') `shouldBe` [0, 0, 1, 1, 2]
      (globallyOrderedEventSequenceNumber <$> events') `shouldBe` [1..5]

insertExampleEvents
  :: (Monad m)
  => EventStore CounterEvent m
  -> m ()
insertExampleEvents store = do
  void $ storeEvents store NoStream uuid1 [Added 1]
  void $ storeEvents store NoStream uuid2 [Added 2, Added 3]
  void $ storeEvents store (ExactVersion 0) uuid1 [Added 4]
  void $ storeEvents store (ExactVersion 1) uuid2 [Added 5]

uuid1 :: UUID
uuid1 = uuidFromInteger 1

uuid2 :: UUID
uuid2 = uuidFromInteger 2
