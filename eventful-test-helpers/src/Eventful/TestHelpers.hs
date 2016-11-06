-- | Common test functionality

module Eventful.TestHelpers
  ( Counter (..)
  , CounterEvent (..)
  , CounterCommand (..)
  , CounterCommandError (..)
  , eventStoreSpec
  , sequencedEventStoreSpec
  , EventBusSpecDelayMilliseconds (..)
  , eventBusSpec
  , module X
  ) where

import Control.Concurrent
import Control.Monad as X
import Control.Monad.Extra
import Control.Monad.IO.Class as X
import Control.Monad.Logger as X
import Data.IORef

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

instance Projection Counter where
  type Event Counter = CounterEvent
  seed = Counter 0
  apply (Counter k) (Added x) = Counter (k + x)

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

instance Aggregate Counter where
  type Command Counter = CounterCommand
  type CommandError Counter = CounterCommandError

  command (Counter k) (Increment n) =
    if k + n <= 100
    then Right $ Added n
    else Left OutOfBounds

  command (Counter k) (Decrement n) =
    if k - n >= 0
    then Right $ Added (-n)
    else Left OutOfBounds

deriveJSON (unPrefix "_counterEvent")  ''CounterEvent
deriveJSON (unPrefix "_counterCommand")  ''CounterCommand
deriveJSON (unPrefix "_counterCommandError")  ''CounterCommandError


-- Test harness for stores

eventStoreSpec
  :: (Serializable Counter serialized, Serializable (Event Counter) serialized, EventStore IO store serialized)
  => IO store -> Spec
eventStoreSpec createStore = do
  context "when the event store is empty" $ do
    store <- runIO createStore

    it "shouldn't have UUIDs" $ do
      getAllUuids store `shouldReturn` []

    it "should return versions of -1 for UUIDs" $ do
      getLatestVersion store nil `shouldReturn` (-1)

  context "when a few events are inserted" $ do
    store <- runIO createStore
    let events = [Added 1, Added 4, Added (-3)]
    _ <- runIO $ storeEvents store (ProjectionId nil :: ProjectionId Counter) events

    it "should return events" $ do
      events' <- getEvents store (ProjectionId nil :: ProjectionId Counter)
      (storedEventEvent <$> events') `shouldBe` events
      --(storedEventSequenceNumber <$> events') `shouldBe` [1, 2, 3]

    it "should return correct event versions" $ do
      getLatestVersion store nil `shouldReturn` 2
      fmap storedEventEvent <$> getEventsFromVersion store (ProjectionId nil :: ProjectionId Counter) (-1)
        >>= (`shouldBe` events)
      fmap storedEventEvent <$> getEventsFromVersion store (ProjectionId nil :: ProjectionId Counter) 1
        >>= (`shouldBe` drop 1 events)

    it "should return the latest projection" $ do
      getLatestProjection store (ProjectionId nil) `shouldReturn` Counter 2

  context "when events from multiple UUIDs are inserted" $ do
    store <- runIO createStore
    (uuid1, uuid2) <- runIO $ insertExampleEvents store
    let (ProjectionId uuid1', ProjectionId uuid2') = (uuid1, uuid2)

    it "should have the correct events for each aggregate" $ do
      events1 <- getEvents store uuid1
      events2 <- getEvents store uuid2
      (storedEventEvent <$> events1) `shouldBe` Added <$> [1, 4]
      (storedEventEvent <$> events2) `shouldBe` Added <$> [2, 3, 5]
      (storedEventProjectionId <$> events1) `shouldBe` [uuid1', uuid1']
      (storedEventProjectionId <$> events2) `shouldBe` [uuid2', uuid2', uuid2']
      (storedEventVersion <$> events1) `shouldBe` [0, 1]
      (storedEventVersion <$> events2) `shouldBe` [0, 1, 2]

    it "should return correct event versions" $ do
      getLatestVersion store (unProjectionId uuid1) `shouldReturn` 1
      getLatestVersion store (unProjectionId uuid2) `shouldReturn` 2
      fmap storedEventEvent <$> getEventsFromVersion store uuid1 0 >>= (`shouldBe` [Added 1, Added 4])
      fmap storedEventEvent <$> getEventsFromVersion store uuid2 1 >>= (`shouldBe` [Added 3, Added 5])

    it "should produce the correct projections" $ do
      getLatestProjection store uuid1 `shouldReturn` Counter 5
      getLatestProjection store uuid2 `shouldReturn` Counter 10


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
    (ProjectionId uuid1, ProjectionId uuid2) <- runIO $ insertExampleEvents store

    it "should have the correct events in global order" $ do
      events' <- getSequencedEvents store 0
      let deserializedEvents = mapMaybe deserialize events'
      (storedEventEvent <$> deserializedEvents) `shouldBe` Added <$> [1..5]
      (storedEventProjectionId <$> deserializedEvents) `shouldBe` [uuid1, uuid2, uuid2, uuid1, uuid2]
      (storedEventVersion <$> deserializedEvents) `shouldBe` [0, 0, 1, 1, 2]
      (storedEventSequenceNumber <$> deserializedEvents) `shouldBe` [1..5]

insertExampleEvents
  :: (Serializable (Event Counter) serialized, EventStore IO store serialized)
  => store -> IO (ProjectionId Counter, ProjectionId Counter)
insertExampleEvents store = do
  let uuid1 = ProjectionId (uuidFromInteger 1) :: ProjectionId Counter
      uuid2 = ProjectionId (uuidFromInteger 2) :: ProjectionId Counter
  void $ storeEvents store uuid1 [Added 1]
  void $ storeEvents store uuid2 [Added 2, Added 3]
  void $ storeEvents store uuid1 [Added 4]
  void $ storeEvents store uuid2 [Added 5]
  return (uuid1, uuid2)

-- Test harness for event buses

-- | Used when testing eventually consistent async buses
newtype EventBusSpecDelayMilliseconds = EventBusSpecDelayMilliseconds Int

eventBusSpec
  :: (Serializable (Event Counter) serialized, EventStore IO store serialized, EventBus IO bus serialized)
  => IO bus -> IO store -> Maybe EventBusSpecDelayMilliseconds -> Spec
eventBusSpec createBus createStore mDelay = do
  let doDelay = whenJust mDelay (\(EventBusSpecDelayMilliseconds delay) -> threadDelay (delay * 1000))

  context "given an event handler that just stores events" $ do
    store <- runIO createStore
    -- Populate store with some sample events
    void $ runIO $ storeEvents store (ProjectionId nil :: ProjectionId Counter) [Added 1, Added 2]

    bus <- runIO createBus
    eventsRef <- runIO $ newIORef []
    let handler event = modifyIORef' eventsRef (\events -> event : events)
    runIO $ registerStoreHandler bus store handler

    it "should have stored the pre-existing events" $ do
      doDelay
      events <- readIORef eventsRef
      length events `shouldBe` 2

    it "should properly transmit events" $ do
      storeAndPublishEvents store bus (ProjectionId nil :: ProjectionId Counter) [Added 3, Added 4]
      doDelay
      events <- readIORef eventsRef
      length events `shouldBe` 4
