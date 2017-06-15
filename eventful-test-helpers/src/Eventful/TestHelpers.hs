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

    it "should return events" $ do
      (store, runargs) <- makeStore
      events' <- runAsIO runargs $ do
        _ <- storeEvents store NoStream nil events
        getEvents store nil Nothing
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return correct event versions" $ do
      (store, runargs) <- makeStore
      (latestVersion, allEvents, someEvents) <- runAsIO runargs $ do
        _ <- storeEvents store NoStream nil events
        (,,) <$>
          getLatestVersion store nil <*>
          getEvents store nil (Just (-1)) <*>
          getEvents store nil (Just 1)
      latestVersion `shouldBe` 2
      (storedEventEvent <$> allEvents) `shouldBe` events
      (storedEventEvent <$> someEvents) `shouldBe` drop 1 events

    it "should return the latest projection" $ do
      (store, runargs) <- makeStore
      projection <- runAsIO runargs $ do
        _ <- storeEvents store NoStream nil events
        getLatestProjection store counterProjection nil
      projection `shouldBe` (Counter 2, 2)

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events for each aggregate" $ do
      (store, runargs) <- makeStore
      (events1, events2) <- runAsIO runargs $ do
        _ <- insertExampleEvents store
        (,) <$> getEvents store uuid1 Nothing <*> getEvents store uuid2 Nothing
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventProjectionId <$> events1) `shouldBe` [uuid1, uuid1]
      (storedEventProjectionId <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (store, runargs) <- makeStore
      (latestVersion1, latestVersion2, events1, events2) <- runAsIO runargs $ do
        _ <- insertExampleEvents store
        (,,,) <$>
          getLatestVersion store uuid1 <*>
          getLatestVersion store uuid2 <*>
          getEvents store uuid1 (Just 0) <*>
          getEvents store uuid2 (Just 1)
      latestVersion1 `shouldBe` 1
      latestVersion2 `shouldBe` 2
      storedEventEvent <$> events1 `shouldBe` [Added 1, Added 4]
      storedEventEvent <$> events2 `shouldBe` [Added 3, Added 5]

    it "should produce the correct projections" $ do
      (store, runargs) <- makeStore
      (proj1, proj2) <- runAsIO runargs $ do
        _ <- insertExampleEvents store
        (,) <$>
          getLatestProjection store counterProjection uuid1 <*>
          getLatestProjection store counterProjection uuid2
      proj1 `shouldBe` (Counter 5, 1)
      proj2 `shouldBe` (Counter 10, 2)

  describe "can handle event storage errors" $ do

    it "rejects some writes when event store isn't created" $ do
      (store, runargs) <- makeStore
      (err1, err2) <- runAsIO runargs $
        (,) <$>
          storeEvents store StreamExists nil [Added 1] <*>
          storeEvents store (ExactVersion 0) nil [Added 1]
      err1 `shouldBe` Just (EventStreamNotAtExpectedVersion (-1))
      err2 `shouldBe` Just (EventStreamNotAtExpectedVersion (-1))

    it "should be able to store events starting with an empty stream" $ do
      (store, runargs) <- makeStore
      runAsIO runargs (storeEvents store NoStream nil [Added 1]) `shouldReturn` Nothing

    it "should reject storing events sometimes with a stream" $ do
      (store, runargs) <- makeStore
      (err1, err2, err3) <- runAsIO runargs $
        (,,) <$>
          storeEvents store NoStream nil [Added 1] <*>
          storeEvents store NoStream nil [Added 1] <*>
          storeEvents store (ExactVersion 1) nil [Added 1]
      err1 `shouldBe` Nothing
      err2 `shouldBe` Just (EventStreamNotAtExpectedVersion 0)
      err3 `shouldBe` Just (EventStreamNotAtExpectedVersion 0)

    it "should accepts storing events sometimes with a stream" $ do
      (store, runargs) <- makeStore
      errors <- runAsIO runargs $
        sequence
          [ storeEvents store NoStream nil [Added 1]
          , storeEvents store AnyVersion nil [Added 1]
          , storeEvents store (ExactVersion 1) nil [Added 1]
          , storeEvents store StreamExists nil [Added 1]
          ]
      errors `shouldBe` [Nothing, Nothing, Nothing, Nothing]

sequencedEventStoreSpec
  :: (Monad m)
  => IO (EventStore CounterEvent m, GloballyOrderedEventStore CounterEvent m, runargs)
  -> (forall a. runargs -> m a -> IO a)
  -> Spec
sequencedEventStoreSpec makeStore runAsIO = do
  context "when the event store is empty" $ do

    it "shouldn't have any events" $ do
      (_, globalStore, runargs) <- makeStore
      length <$> runAsIO runargs (getSequencedEvents globalStore 0) `shouldReturn` 0

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events in global order" $ do
      (store, globalStore, runargs) <- makeStore
      events' <- runAsIO runargs $ do
        insertExampleEvents store
        getSequencedEvents globalStore 0
      (globallyOrderedEventEvent <$> events') `shouldBe` Added <$> [1..5]
      (globallyOrderedEventProjectionId <$> events') `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (globallyOrderedEventVersion <$> events') `shouldBe` [0, 0, 1, 1, 2]
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
