module EventSourcing.Store.Memory
  ( EventStoreMap (..)
  , eventStoreMapTVar
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Dynamic
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import EventSourcing.Store.Class
import EventSourcing.UUID

newtype EventStoreMap = EventStoreMap { unEventStoreMap :: Map UUID [Dynamic] }
  deriving (Show)

eventStoreMapTVar :: IO (TVar EventStoreMap)
eventStoreMapTVar = newTVarIO (EventStoreMap Map.empty)

lookupEventStoreMapRaw :: EventStoreMap -> UUID -> [StoredEvent Dynamic]
lookupEventStoreMapRaw (EventStoreMap esmap) uuid =
  let dynEvents = maybe [] toList $ Map.lookup uuid esmap
  in zipWith (StoredEvent uuid) [0..] dynEvents

storedEventFromDyn :: (Typeable event) => StoredEvent Dynamic -> Maybe (StoredEvent event)
storedEventFromDyn (StoredEvent uuid version dynEvent) = StoredEvent uuid version <$> fromDynamic dynEvent

lookupEventStoreMap :: (Typeable event) => EventStoreMap -> UUID -> [StoredEvent event]
lookupEventStoreMap store uuid = mapMaybe storedEventFromDyn (lookupEventStoreMapRaw store uuid)

storeEventStoreMapRaw :: EventStoreMap -> UUID -> [Dynamic] -> (EventStoreMap, [StoredEvent Dynamic])
storeEventStoreMapRaw (EventStoreMap esmap) uuid events =
  let oldEvents = maybe [] toList $ Map.lookup uuid esmap
      newMap = EventStoreMap $ Map.insert uuid (oldEvents ++ events) esmap
      versStart = EventVersion (length oldEvents)
      storedEvents = zipWith (StoredEvent uuid) [versStart..] events
  in (newMap, storedEvents)

storeEventStoreMap :: (Typeable event) => EventStoreMap -> UUID -> [event] -> (EventStoreMap, [StoredEvent event])
storeEventStoreMap store uuid events =
  let (newMap, rawStoredEvents) = storeEventStoreMapRaw store uuid (toDyn <$> events)
      storedEvents = mapMaybe storedEventFromDyn rawStoredEvents
  in (newMap, storedEvents)

instance (MonadIO m) => RawEventStore m (TVar EventStoreMap) Dynamic where
  getUuids tvar = liftIO $ Map.keys . unEventStoreMap <$> readTVarIO tvar
  getEvents tvar uuid = liftIO $ flip lookupEventStoreMapRaw uuid <$> readTVarIO tvar
  storeEvents tvar uuid events = liftIO . atomically $ do
    store <- readTVar tvar
    let (newMap, storedEvents) = storeEventStoreMapRaw store uuid events
    writeTVar tvar newMap
    return storedEvents
  latestEventVersion tvar uuid = liftIO $ do
    store <- readTVarIO tvar
    return $ EventVersion . (+) (-1) . length $ lookupEventStoreMapRaw store uuid

instance (Typeable event, MonadIO m) => SerializedEventStore m (TVar EventStoreMap) Dynamic event where
  getSerializedEvents tvar uuid = liftIO $ do
    store <- readTVarIO tvar
    return $ lookupEventStoreMap store uuid
  storeSerializedEvents tvar uuid events = liftIO . atomically $ do
    store <- readTVar tvar
    let (newMap, storedEvents) = storeEventStoreMap store uuid events
    writeTVar tvar newMap
    return storedEvents
