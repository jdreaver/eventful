{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Common test functionality

module Eventful.TestHelpers
  ( Counter (..)
  , CounterProjection
  , counterProjection
  , CounterCommandHandler
  , counterCommandHandler
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

-- | Example Projection/CommandHandler
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

type CounterCommandHandler = CommandHandler Counter CounterEvent CounterCommand

counterCommandHandler :: CounterCommandHandler
counterCommandHandler = CommandHandler counterCommand counterProjection

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
  EventStoreRunner (forall a. (VersionedEventStoreWriter m CounterEvent -> VersionedEventStoreReader m CounterEvent -> m a) -> IO a)

eventStoreSpec
  :: (Monad m)
  => EventStoreRunner m
  -> Spec
eventStoreSpec (EventStoreRunner withStore) = do
  let
    withStoreExampleEvents action = withStore $ \writer reader -> do
      _ <- insertExampleEvents writer
      action writer reader

  context "when a few events are inserted" $ do
    let
      sampleEvents = [Added 1, Added 4, Added (-3), Added 5]
      withStore' action = withStore $ \writer reader -> do
        _ <- storeEvents writer nil NoStream sampleEvents
        action writer reader

    it "should return events" $ do
      events' <- withStore' $ \_ reader -> getEvents reader (allEvents nil)
      (streamEventEvent <$> events') `shouldBe` sampleEvents

    it "should return correct event versions" $ do
      events <- withStore' $ \_ reader -> getEvents reader (allEvents nil)
      (streamEventPosition <$> events) `shouldBe` [0, 1, 2, 3]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore' $ \_ reader ->
        (,,,) <$>
          getEvents reader (eventsUntil nil 1) <*>
          getEvents reader (eventsStartingAtUntil nil 1 2) <*>
          getEvents reader (eventsStartingAt nil 2) <*>
          getEvents reader (eventsStartingAtTakeLimit nil 0 2)
      (streamEventEvent <$> firstEvents) `shouldBe` take 2 sampleEvents
      (streamEventEvent <$> middleEvents) `shouldBe` take 2 (drop 1 sampleEvents)
      (streamEventEvent <$> laterEvents) `shouldBe` drop 2 sampleEvents
      (streamEventEvent <$> maxEvents) `shouldBe` take 2 sampleEvents

    it "should return the latest projection" $ do
      projection <- withStore' $ \_ reader ->
        getLatestStreamProjection reader (versionedStreamProjection nil counterProjection)
      streamProjectionState projection `shouldBe` Counter 7
      streamProjectionPosition projection `shouldBe` 3
      streamProjectionKey projection `shouldBe` nil

    it "should return the latest projection with some starting StreamProjection" $ do
      projection <- withStore' $ \_ reader -> do
        initialEvents <- getEvents reader (eventsUntil nil 1)
        let initialProjection = latestProjection counterProjection (streamEventEvent <$> initialEvents)
        getLatestStreamProjection reader (StreamProjection nil 1 counterProjection initialProjection)
      streamProjectionState projection `shouldBe` Counter 7
      streamProjectionPosition projection `shouldBe` 3
      streamProjectionKey projection `shouldBe` nil

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events for each stream" $ do
      (events1, events2) <- withStoreExampleEvents $ \_ reader ->
        (,) <$> getEvents reader (allEvents uuid1) <*> getEvents reader (allEvents uuid2)
      (streamEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (streamEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (streamEventKey <$> events1) `shouldBe` [uuid1, uuid1]
      (streamEventKey <$> events2) `shouldBe` [uuid2, uuid2, uuid2]
      (streamEventPosition <$> events1) `shouldBe` [0, 1]
      (streamEventPosition <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      (events1, events2) <- withStoreExampleEvents $ \_ reader ->
        (,) <$>
          getEvents reader (allEvents uuid1) <*>
          getEvents reader (allEvents uuid2)
      streamEventEvent <$> events1 `shouldBe` [Added 1, Added 4]
      streamEventEvent <$> events2 `shouldBe` [Added 2, Added 3, Added 5]

    it "should return correct events with queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStoreExampleEvents $ \_ reader ->
        (,,,) <$>
          getEvents reader (eventsUntil uuid1 1) <*>
          getEvents reader (eventsStartingAtUntil uuid2 1 2) <*>
          getEvents reader (eventsStartingAt uuid2 2) <*>
          getEvents reader (eventsStartingAtTakeLimit uuid1 1 1)
      (streamEventEvent <$> firstEvents) `shouldBe` [Added 1, Added 4]
      (streamEventEvent <$> middleEvents) `shouldBe` [Added 3, Added 5]
      (streamEventEvent <$> laterEvents) `shouldBe` [Added 5]
      (streamEventEvent <$> maxEvents) `shouldBe` [Added 4]

    it "should produce the correct projections" $ do
      (proj1, proj2) <- withStoreExampleEvents $ \_ reader ->
        (,) <$>
          getLatestStreamProjection reader (versionedStreamProjection uuid1 counterProjection) <*>
          getLatestStreamProjection reader (versionedStreamProjection uuid2 counterProjection)
      (streamProjectionState proj1, streamProjectionPosition proj1) `shouldBe` (Counter 5, 1)
      (streamProjectionState proj2, streamProjectionPosition proj2) `shouldBe` (Counter 10, 2)

  describe "can handle event storage errors" $ do

    it "rejects some writes when event store isn't created" $ do
      (err1, err2) <- withStore $ \writer _ ->
        (,) <$>
          storeEvents writer nil StreamExists [Added 1] <*>
          storeEvents writer nil (ExactPosition 0) [Added 1]
      err1 `shouldBe` Left (EventStreamNotAtExpectedVersion (-1))
      err2 `shouldBe` Left (EventStreamNotAtExpectedVersion (-1))

    it "should be able to store events starting with an empty stream" $ do
      withStore (\writer _ -> storeEvents writer nil NoStream [Added 1]) `shouldReturn` Right 0

    it "should reject storing events sometimes with a stream" $ do
      (err1, err2, err3) <- withStore $ \writer _ ->
        (,,) <$>
          storeEvents writer nil NoStream [Added 1] <*>
          storeEvents writer nil NoStream [Added 1] <*>
          storeEvents writer nil (ExactPosition 1) [Added 1]
      err1 `shouldBe` Right 0
      err2 `shouldBe` Left (EventStreamNotAtExpectedVersion 0)
      err3 `shouldBe` Left (EventStreamNotAtExpectedVersion 0)

    it "should accepts storing events sometimes with a stream" $ do
      errors <- withStore $ \writer _ ->
        sequence
          [ storeEvents writer nil NoStream [Added 1]
          , storeEvents writer nil AnyPosition [Added 1]
          , storeEvents writer nil (ExactPosition 1) [Added 1]
          , storeEvents writer nil StreamExists [Added 1]
          ]
      errors `shouldBe` [Right 0, Right 1, Right 2, Right 3]

newtype GlobalStreamEventStoreRunner m =
  GlobalStreamEventStoreRunner
  (forall a. (VersionedEventStoreWriter m CounterEvent -> GlobalEventStoreReader m CounterEvent -> m a) -> IO a)

globalStreamEventStoreSpec
  :: (Monad m)
  => GlobalStreamEventStoreRunner m
  -> Spec
globalStreamEventStoreSpec (GlobalStreamEventStoreRunner withStore) = do
  context "when the event store is empty" $ do

    it "shouldn't have any events" $ do
      events <- withStore (\_ globalReader -> getEvents globalReader (allEvents ()))
      length events `shouldBe` 0

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct events in global order" $ do
      events <- withStore $ \writer globalReader -> do
        insertExampleEvents writer
        getEvents globalReader (allEvents ())
      (streamEventEvent . streamEventEvent <$> events) `shouldBe` Added <$> [1..5]
      (streamEventKey . streamEventEvent <$> events) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (streamEventPosition . streamEventEvent <$> events) `shouldBe` [0, 0, 1, 1, 2]
      (streamEventPosition <$> events) `shouldBe` [1..5]

    it "should work with global projections" $ do
      (proj1, proj2) <- withStore $ \writer globalReader -> do
        insertExampleEvents writer
        p1 <- getLatestStreamProjection globalReader (globalStreamProjection counterGlobalProjection)
        _ <- storeEvents writer uuid1 AnyPosition [Added 10, Added 20]
        p2 <- getLatestStreamProjection globalReader p1
        return (p1, p2)

      streamProjectionPosition proj1 `shouldBe` 5
      streamProjectionPosition proj2 `shouldBe` 7

    it "should handle queries" $ do
      (firstEvents, middleEvents, laterEvents, maxEvents) <- withStore $ \writer globalReader -> do
        insertExampleEvents writer
        (,,,) <$>
          getEvents globalReader (eventsUntil () 2) <*>
          getEvents globalReader (eventsStartingAtUntil () 2 3) <*>
          getEvents globalReader (eventsStartingAt () 3) <*>
          getEvents globalReader (eventsStartingAtTakeLimit () 2 3)

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
  => VersionedEventStoreWriter m CounterEvent
  -> m ()
insertExampleEvents store = do
  void $ storeEvents store uuid1 NoStream [Added 1]
  void $ storeEvents store uuid2 NoStream [Added 2, Added 3]
  void $ storeEvents store uuid1 (ExactPosition 0) [Added 4]
  void $ storeEvents store uuid2 (ExactPosition 1) [Added 5]

uuid1 :: UUID
uuid1 = uuidFromInteger 1

uuid2 :: UUID
uuid2 = uuidFromInteger 2

newtype VersionedProjectionCacheRunner m =
  VersionedProjectionCacheRunner
  (forall a.
   (  VersionedEventStoreWriter m CounterEvent
   -> VersionedEventStoreReader m CounterEvent
   -> VersionedProjectionCache Counter m -> m a)
   -> IO a
  )

versionedProjectionCacheSpec
  :: (Monad m)
  => VersionedProjectionCacheRunner m
  -> Spec
versionedProjectionCacheSpec (VersionedProjectionCacheRunner withStoreAndCache) = do
  context "when the store is empty" $ do

    it "should be able to store and load simple projections" $ do
      snapshot <- withStoreAndCache $ \_ _ cache -> do
        storeProjectionSnapshot cache nil 4 (Counter 100)
        loadProjectionSnapshot cache nil
      snapshot `shouldBe` Just (4, Counter 100)

  context "when the store has some events in one stream" $ do

    it "should load from a stream of events" $ do
      snapshot <- withStoreAndCache $ \writer reader cache -> do
        _ <- storeEvents writer nil AnyPosition [Added 1, Added 2]
        getLatestVersionedProjectionWithCache reader cache (versionedStreamProjection nil counterProjection)
      streamProjectionPosition snapshot `shouldBe` 1
      streamProjectionState snapshot `shouldBe` Counter 3

    it "should work with updateProjectionCache" $ do
      snapshot <- withStoreAndCache $ \writer reader cache -> do
        _ <- storeEvents writer nil AnyPosition [Added 1, Added 2, Added 3]
        updateProjectionCache reader cache (versionedStreamProjection nil counterProjection)
        getLatestVersionedProjectionWithCache reader cache (versionedStreamProjection nil counterProjection)
      streamProjectionKey snapshot `shouldBe` nil
      streamProjectionPosition snapshot `shouldBe` 2
      streamProjectionState snapshot `shouldBe` Counter 6

newtype GlobalStreamProjectionCacheRunner m =
  GlobalStreamProjectionCacheRunner
  (forall a.
    (  VersionedEventStoreWriter m CounterEvent
    -> GlobalEventStoreReader m CounterEvent
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
      snapshot <- withStoreAndCache $ \writer globalReader cache -> do
        _ <- storeEvents writer nil AnyPosition [Added 1, Added 2]
        getLatestGlobalProjectionWithCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
      streamProjectionPosition snapshot `shouldBe` 2
      streamProjectionState snapshot `shouldBe` Counter 3

    it "should work with updateGlobalProjectionCache" $ do
      snapshot <- withStoreAndCache $ \writer globalReader cache -> do
        _ <- storeEvents writer nil AnyPosition [Added 1, Added 2, Added 3]
        updateGlobalProjectionCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
        getLatestGlobalProjectionWithCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
      streamProjectionPosition snapshot `shouldBe` 3
      streamProjectionState snapshot `shouldBe` Counter 6

  context "when events from multiple UUIDs are inserted" $ do

    it "should have the correct cached projection value" $ do
      snapshot <- withStoreAndCache $ \writer globalReader cache -> do
        insertExampleEvents writer
        updateGlobalProjectionCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
        getLatestGlobalProjectionWithCache globalReader cache (globalStreamProjection counterGlobalProjection) "key"
      streamProjectionPosition snapshot `shouldBe` 5
      streamProjectionState snapshot `shouldBe` Counter 15
