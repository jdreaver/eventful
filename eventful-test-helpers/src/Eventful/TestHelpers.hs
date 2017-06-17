{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Common test functionality

module Eventful.TestHelpers
  ( Counter (..)
  , CounterProjection
  , counterProjection
  , CounterAggregate
  , counterAggregate
  , CounterEvent (..)
  , CounterCommand (..)
  , EventStoreRunner (..)
  , GloballyOrderedEventStoreRunner (..)
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

newtype EventStoreRunner m =
  EventStoreRunner (forall a. (EventStore CounterEvent m -> m a) -> IO a)
newtype GloballyOrderedEventStoreRunner m =
  GloballyOrderedEventStoreRunner (forall a. (EventStore CounterEvent m -> GloballyOrderedEventStore CounterEvent m -> m a) -> IO a)

eventStoreSpec
  :: (Monad m)
  => EventStoreRunner m
  -> Spec
eventStoreSpec (EventStoreRunner withStore) = do
  let
    withStoreExampleEvents action = withStore $ \store -> do
      _ <- insertExampleEvents store
      action store

  context "when the event store is empty" $ do

    it "should return versions of -1 for a UUID" $ do
      withStore (\store -> getLatestVersion store nil) `shouldReturn` (-1)

  context "when a few events are inserted" $ do
    let
      sampleEvents = [Added 1, Added 4, Added (-3), Added 5]
      withStore' action = withStore $ \store -> do
        _ <- storeEvents store NoStream nil sampleEvents
        action store

    it "should return events" $ do
      events' <- withStore' $ \store -> getEvents store nil allEvents
      (storedEventEvent <$> events') `shouldBe` sampleEvents

    it "should return correct event versions" $ do
      (latestVersion, events) <- withStore' $ \store ->
        (,) <$>
          getLatestVersion store nil <*>
          getEvents store nil allEvents
      latestVersion `shouldBe` 3
      (storedEventVersion <$> events) `shouldBe` [0, 1, 2, 3]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore' $ \store ->
        (,,,) <$>
          getEvents store nil (eventsUntil 1) <*>
          getEvents store nil (eventsStartingAtUntil 1 2) <*>
          getEvents store nil (eventsStartingAt 2) <*>
          getEvents store nil (eventsStartingAtTakeLimit 0 2)
      (storedEventEvent <$> firstEvents) `shouldBe` take 2 sampleEvents
      (storedEventEvent <$> middleEvents) `shouldBe` take 2 (drop 1 sampleEvents)
      (storedEventEvent <$> laterEvents) `shouldBe` drop 2 sampleEvents
      (storedEventEvent <$> maxEvents) `shouldBe` take 2 sampleEvents

    it "should return the latest projection" $ do
      projection <- withStore' $ \store ->
        getLatestProjection store counterProjection nil
      projection `shouldBe` (Counter 7, 3)

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events for each aggregate" $ do
      (events1, events2) <- withStoreExampleEvents $ \store ->
        (,) <$> getEvents store uuid1 allEvents <*> getEvents store uuid2 allEvents
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventProjectionId <$> events1) `shouldBe` [uuid1, uuid1]
      (storedEventProjectionId <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (latestVersion1, latestVersion2, events1, events2) <- withStoreExampleEvents $ \store ->
        (,,,) <$>
          getLatestVersion store uuid1 <*>
          getLatestVersion store uuid2 <*>
          getEvents store uuid1 allEvents <*>
          getEvents store uuid2 allEvents
      latestVersion1 `shouldBe` 1
      latestVersion2 `shouldBe` 2
      storedEventEvent <$> events1 `shouldBe` [Added 1, Added 4]
      storedEventEvent <$> events2 `shouldBe` [Added 2, Added 3, Added 5]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStoreExampleEvents $ \store ->
        (,,,) <$>
          getEvents store uuid1 (eventsUntil 1) <*>
          getEvents store uuid2 (eventsStartingAtUntil 1 2) <*>
          getEvents store uuid2 (eventsStartingAt 2) <*>
          getEvents store uuid1 (eventsStartingAtTakeLimit 1 1)
      (storedEventEvent <$> firstEvents) `shouldBe` [Added 1, Added 4]
      (storedEventEvent <$> middleEvents) `shouldBe` [Added 3, Added 5]
      (storedEventEvent <$> laterEvents) `shouldBe` [Added 5]
      (storedEventEvent <$> maxEvents) `shouldBe` [Added 4]

    it "should produce the correct projections" $ do
      (proj1, proj2) <- withStoreExampleEvents $ \store ->
        (,) <$>
          getLatestProjection store counterProjection uuid1 <*>
          getLatestProjection store counterProjection uuid2
      proj1 `shouldBe` (Counter 5, 1)
      proj2 `shouldBe` (Counter 10, 2)

  describe "can handle event storage errors" $ do

    it "rejects some writes when event store isn't created" $ do
      (err1, err2) <- withStore $ \store -> do
        (,) <$>
          storeEvents store StreamExists nil [Added 1] <*>
          storeEvents store (ExactVersion 0) nil [Added 1]
      err1 `shouldBe` Just (EventStreamNotAtExpectedVersion (-1))
      err2 `shouldBe` Just (EventStreamNotAtExpectedVersion (-1))

    it "should be able to store events starting with an empty stream" $ do
      withStore (\store -> storeEvents store NoStream nil [Added 1]) `shouldReturn` Nothing

    it "should reject storing events sometimes with a stream" $ do
      (err1, err2, err3) <- withStore $ \store ->
        (,,) <$>
          storeEvents store NoStream nil [Added 1] <*>
          storeEvents store NoStream nil [Added 1] <*>
          storeEvents store (ExactVersion 1) nil [Added 1]
      err1 `shouldBe` Nothing
      err2 `shouldBe` Just (EventStreamNotAtExpectedVersion 0)
      err3 `shouldBe` Just (EventStreamNotAtExpectedVersion 0)

    it "should accepts storing events sometimes with a stream" $ do
      errors <- withStore $ \store ->
        sequence
          [ storeEvents store NoStream nil [Added 1]
          , storeEvents store AnyVersion nil [Added 1]
          , storeEvents store (ExactVersion 1) nil [Added 1]
          , storeEvents store StreamExists nil [Added 1]
          ]
      errors `shouldBe` [Nothing, Nothing, Nothing, Nothing]

sequencedEventStoreSpec
  :: (Monad m)
  => GloballyOrderedEventStoreRunner m
  -> Spec
sequencedEventStoreSpec (GloballyOrderedEventStoreRunner withStore) = do
  context "when the event store is empty" $ do

    it "shouldn't have any events" $ do
      events <- withStore (\_ globalStore -> getSequencedEvents globalStore allEvents)
      length events `shouldBe` 0

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events in global order" $ do
      events <- withStore $ \store globalStore -> do
        insertExampleEvents store
        getSequencedEvents globalStore allEvents
      (globallyOrderedEventEvent <$> events) `shouldBe` Added <$> [1..5]
      (globallyOrderedEventProjectionId <$> events) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (globallyOrderedEventVersion <$> events) `shouldBe` [0, 0, 1, 1, 2]
      (globallyOrderedEventSequenceNumber <$> events) `shouldBe` [1..5]

    it "should handle queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore $ \store globalStore -> do
        insertExampleEvents store
        (,,,) <$>
          getSequencedEvents globalStore (eventsUntil 2) <*>
          getSequencedEvents globalStore (eventsStartingAtUntil 2 3) <*>
          getSequencedEvents globalStore (eventsStartingAt 3) <*>
          getSequencedEvents globalStore (eventsStartingAtTakeLimit 2 3)

      (globallyOrderedEventEvent <$> firstEvents) `shouldBe` Added <$> [1..2]
      (globallyOrderedEventEvent <$> middleEvents) `shouldBe` Added <$> [2..3]
      (globallyOrderedEventEvent <$> laterEvents) `shouldBe` Added <$> [3..5]
      (globallyOrderedEventEvent <$> maxEvents) `shouldBe` Added <$> [2..4]

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
