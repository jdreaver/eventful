{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , GlobalStreamEventStoreRunner (..)
  , eventStoreSpec
  , globalStreamEventStoreSpec
  , VersionedProjectionCacheRunner (..)
  , versionedProjectionCacheSpec
  , GlobalStreamProjectionCacheRunner (..)
  , globalStreamProjectionCacheSpec
  , Text
  , module X
  ) where

import Control.Monad as X
import Control.Monad.IO.Class as X
import Control.Monad.Logger as X

import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Text (Text)
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

counterGlobalProjection :: Projection Counter (VersionedStreamEvent CounterEvent)
counterGlobalProjection =
  Projection
  (Counter 0)
  (\(Counter k) (StreamEvent _ _ (Added x)) -> Counter (k + x))

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
newtype GlobalStreamEventStoreRunner m =
  GlobalStreamEventStoreRunner (forall a. (EventStore CounterEvent m -> GlobalStreamEventStore CounterEvent m -> m a) -> IO a)

eventStoreSpec
  :: (Monad m)
  => EventStoreRunner m
  -> Spec
eventStoreSpec (EventStoreRunner withStore) = do
  let
    withStoreExampleEvents action = withStore $ \store -> do
      _ <- insertExampleEvents store
      action store

  context "when a few events are inserted" $ do
    let
      sampleEvents = [Added 1, Added 4, Added (-3), Added 5]
      withStore' action = withStore $ \store -> do
        _ <- storeEvents store NoStream nil sampleEvents
        action store

    it "should return events" $ do
      events' <- withStore' $ \store -> getEvents store (allEvents nil)
      (streamEventEvent <$> events') `shouldBe` sampleEvents

    it "should return correct event versions" $ do
      events <- withStore' $ \store -> getEvents store (allEvents nil)
      (streamEventPosition <$> events) `shouldBe` [0, 1, 2, 3]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore' $ \store ->
        (,,,) <$>
          getEvents store (eventsUntil nil 1) <*>
          getEvents store (eventsStartingAtUntil nil 1 2) <*>
          getEvents store (eventsStartingAt nil 2) <*>
          getEvents store (eventsStartingAtTakeLimit nil 0 2)
      (streamEventEvent <$> firstEvents) `shouldBe` take 2 sampleEvents
      (streamEventEvent <$> middleEvents) `shouldBe` take 2 (drop 1 sampleEvents)
      (streamEventEvent <$> laterEvents) `shouldBe` drop 2 sampleEvents
      (streamEventEvent <$> maxEvents) `shouldBe` take 2 sampleEvents

    it "should return the latest projection" $ do
      projection <- withStore' $ \store ->
        getLatestVersionedProjection store (versionedStreamProjection nil counterProjection)
      streamProjectionState projection `shouldBe` Counter 7
      streamProjectionPosition projection `shouldBe` 3
      streamProjectionKey projection `shouldBe` nil

    it "should return the latest projection with some starting StreamProjection" $ do
      projection <- withStore' $ \store -> do
        initialEvents <- getEvents store (eventsUntil nil 1)
        let initialProjection = latestProjection counterProjection (streamEventEvent <$> initialEvents)
        getLatestVersionedProjection store (StreamProjection nil 1 counterProjection initialProjection)
      streamProjectionState projection `shouldBe` Counter 7
      streamProjectionPosition projection `shouldBe` 3
      streamProjectionKey projection `shouldBe` nil

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events for each aggregate" $ do
      (events1, events2) <- withStoreExampleEvents $ \store ->
        (,) <$> getEvents store (allEvents uuid1) <*> getEvents store (allEvents uuid2)
      (streamEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (streamEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (streamEventKey <$> events1) `shouldBe` [uuid1, uuid1]
      (streamEventKey <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (streamEventPosition <$> events1) `shouldBe` [0, 1]
      (streamEventPosition <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (events1, events2) <- withStoreExampleEvents $ \store ->
        (,) <$>
          getEvents store (allEvents uuid1) <*>
          getEvents store (allEvents uuid2)
      streamEventEvent <$> events1 `shouldBe` [Added 1, Added 4]
      streamEventEvent <$> events2 `shouldBe` [Added 2, Added 3, Added 5]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStoreExampleEvents $ \store ->
        (,,,) <$>
          getEvents store (eventsUntil uuid1 1) <*>
          getEvents store (eventsStartingAtUntil uuid2 1 2) <*>
          getEvents store (eventsStartingAt uuid2 2) <*>
          getEvents store (eventsStartingAtTakeLimit uuid1 1 1)
      (streamEventEvent <$> firstEvents) `shouldBe` [Added 1, Added 4]
      (streamEventEvent <$> middleEvents) `shouldBe` [Added 3, Added 5]
      (streamEventEvent <$> laterEvents) `shouldBe` [Added 5]
      (streamEventEvent <$> maxEvents) `shouldBe` [Added 4]

    it "should produce the correct projections" $ do
      (proj1, proj2) <- withStoreExampleEvents $ \store ->
        (,) <$>
          getLatestVersionedProjection store (versionedStreamProjection uuid1 counterProjection) <*>
          getLatestVersionedProjection store (versionedStreamProjection uuid2 counterProjection)
      (streamProjectionState proj1, streamProjectionPosition proj1) `shouldBe` (Counter 5, 1)
      (streamProjectionState proj2, streamProjectionPosition proj2) `shouldBe` (Counter 10, 2)

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

globalStreamEventStoreSpec
  :: (Monad m)
  => GlobalStreamEventStoreRunner m
  -> Spec
globalStreamEventStoreSpec (GlobalStreamEventStoreRunner withStore) = do
  context "when the event store is empty" $ do

    it "shouldn't have any events" $ do
      events <- withStore (\_ globalStore -> getGlobalEvents globalStore (allEvents ()))
      length events `shouldBe` 0

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events in global order" $ do
      events <- withStore $ \store globalStore -> do
        insertExampleEvents store
        getGlobalEvents globalStore (allEvents ())
      (streamEventEvent . streamEventEvent <$> events) `shouldBe` Added <$> [1..5]
      (streamEventKey . streamEventEvent <$> events) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (streamEventPosition . streamEventEvent <$> events) `shouldBe` [0, 0, 1, 1, 2]
      (streamEventPosition <$> events) `shouldBe` [1..5]

    it "should work with global projections" $ do
      (proj1, proj2) <- withStore $ \store globalStore -> do
        insertExampleEvents store
        p1 <- getLatestGlobalProjection globalStore (globalStreamProjection counterGlobalProjection)
        _ <- storeEvents store AnyVersion uuid1 [Added 10, Added 20]
        p2 <- getLatestGlobalProjection globalStore p1
        return (p1, p2)

      streamProjectionPosition proj1 `shouldBe` 5
      streamProjectionPosition proj2 `shouldBe` 7

    it "should handle queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore $ \store globalStore -> do
        insertExampleEvents store
        (,,,) <$>
          getGlobalEvents globalStore (eventsUntil () 2) <*>
          getGlobalEvents globalStore (eventsStartingAtUntil () 2 3) <*>
          getGlobalEvents globalStore (eventsStartingAt () 3) <*>
          getGlobalEvents globalStore (eventsStartingAtTakeLimit () 2 3)

      (streamEventEvent . streamEventEvent <$> firstEvents) `shouldBe` Added <$> [1..2]
      (streamEventPosition <$> firstEvents) `shouldBe` [1..2]
      (streamEventEvent . streamEventEvent <$> middleEvents) `shouldBe` Added <$> [2..3]
      (streamEventPosition <$> middleEvents) `shouldBe` [2..3]
      (streamEventEvent . streamEventEvent <$> laterEvents) `shouldBe` Added <$> [3..5]
      (streamEventPosition <$> laterEvents) `shouldBe` [3..5]
      (streamEventEvent . streamEventEvent <$> maxEvents) `shouldBe` Added <$> [2..4]
      (streamEventPosition <$> maxEvents) `shouldBe` [2..4]

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

newtype VersionedProjectionCacheRunner m =
  VersionedProjectionCacheRunner (forall a. (EventStore CounterEvent m -> VersionedProjectionCache Counter m -> m a) -> IO a)

versionedProjectionCacheSpec
  :: (Monad m)
  => VersionedProjectionCacheRunner m
  -> Spec
versionedProjectionCacheSpec (VersionedProjectionCacheRunner withStoreAndCache) = do
  context "when the store is empty" $ do

    it "should be able to store and load simple projections" $ do
      snapshot <- withStoreAndCache $ \_ cache -> do
        storeProjectionSnapshot cache nil 4 (Counter 100)
        loadProjectionSnapshot cache nil
      snapshot `shouldBe` Just (4, Counter 100)

  context "when the store has some events in one stream" $ do

    it "should load from a stream of events" $ do
      snapshot <- withStoreAndCache $ \store cache -> do
        _ <- storeEvents store AnyVersion nil [Added 1, Added 2]
        getLatestVersionedProjectionWithCache store cache (versionedStreamProjection nil counterProjection)
      streamProjectionPosition snapshot `shouldBe` 1
      streamProjectionState snapshot `shouldBe` Counter 3

    it "should work with updateProjectionCache" $ do
      snapshot <- withStoreAndCache $ \store cache -> do
        _ <- storeEvents store AnyVersion nil [Added 1, Added 2, Added 3]
        updateProjectionCache store cache (versionedStreamProjection nil counterProjection)
        getLatestVersionedProjectionWithCache store cache (versionedStreamProjection nil counterProjection)
      streamProjectionKey snapshot `shouldBe` nil
      streamProjectionPosition snapshot `shouldBe` 2
      streamProjectionState snapshot `shouldBe` Counter 6

newtype GlobalStreamProjectionCacheRunner m =
  GlobalStreamProjectionCacheRunner
  (forall a.
    ( EventStore CounterEvent m
    -> GlobalStreamEventStore CounterEvent m
    -> GlobalStreamProjectionCache Text Counter m -> m a
    ) -> IO a)

globalStreamProjectionCacheSpec
  :: (Monad m)
  => GlobalStreamProjectionCacheRunner m
  -> Spec
globalStreamProjectionCacheSpec (GlobalStreamProjectionCacheRunner withStoreAndCache) = do
  context "when the store is empty" $ do

    it "should be able to store and load simple projections" $ do
      snapshot <- withStoreAndCache $ \_ _ cache -> do
        storeProjectionSnapshot cache "key" 4 (Counter 100)
        loadProjectionSnapshot cache "key"
      snapshot `shouldBe` Just (4, Counter 100)

  context "when the store has some events in one stream" $ do

    it "should load from a global stream of events" $ do
      snapshot <- withStoreAndCache $ \store globalStore cache -> do
        _ <- storeEvents store AnyVersion nil [Added 1, Added 2]
        getLatestGlobalProjectionWithCache globalStore cache (globalStreamProjection counterGlobalProjection) "key"
      streamProjectionPosition snapshot `shouldBe` 2
      streamProjectionState snapshot `shouldBe` Counter 3

    it "should work with updateGlobalProjectionCache" $ do
      snapshot <- withStoreAndCache $ \store globalStore cache -> do
        _ <- storeEvents store AnyVersion nil [Added 1, Added 2, Added 3]
        updateGlobalProjectionCache globalStore cache (globalStreamProjection counterGlobalProjection) "key"
        getLatestGlobalProjectionWithCache globalStore cache (globalStreamProjection counterGlobalProjection) "key"
      streamProjectionPosition snapshot `shouldBe` 3
      streamProjectionState snapshot `shouldBe` Counter 6

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct cached projection value" $ do
      snapshot <- withStoreAndCache $ \store globalStore cache -> do
        insertExampleEvents store
        updateGlobalProjectionCache globalStore cache (globalStreamProjection counterGlobalProjection) "key"
        getLatestGlobalProjectionWithCache globalStore cache (globalStreamProjection counterGlobalProjection) "key"
      streamProjectionPosition snapshot `shouldBe` 5
      streamProjectionState snapshot `shouldBe` Counter 15
