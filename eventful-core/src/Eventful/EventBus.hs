module Eventful.EventBus
  ( synchronousEventBusWrapper
  , storeAndPublishEvents
  ) where

import Eventful.Store.Class
import Eventful.UUID

-- | In-memory synchronous event bus. This takes a list of event handlers and
-- wraps an event store. When events are stored the handlers are called with
-- the new events.
synchronousEventBusWrapper
  :: (Monad m)
  => EventStore serialized m
  -> [UUID -> serialized -> m ()]
  -> EventStore serialized m
synchronousEventBusWrapper store handlers =
  EventStore
  { getLatestVersion = getLatestVersion store
  , getEvents = getEvents store
  , storeEvents = storeAndPublishEvents store handlers
  }

-- | Stores events in the store and them publishes them to the event handlers.
storeAndPublishEvents
  :: (Monad m)
  => EventStore serialized m
  -> [UUID -> serialized -> m ()]
  -> ExpectedVersion
  -> UUID
  -> [serialized]
  -> m (Maybe EventWriteError)
storeAndPublishEvents store handlers expectedVersion uuid events = do
  result <- storeEvents store expectedVersion uuid events
  case result of
    Just err -> return $ Just err
    Nothing -> do
      mapM_ (\event -> mapM_ (\handler -> handler uuid event) handlers) events
      return Nothing
