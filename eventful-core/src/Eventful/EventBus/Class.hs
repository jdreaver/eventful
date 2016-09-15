module Eventful.EventBus.Class
  ( EventBus (..)
  , eventBusRegisterStoreHandler
  , registerReadModel
  , storeAndPublishEvent
  , runAggregateCommand
  ) where

import Control.Monad.IO.Class

import Eventful.Aggregate
import Eventful.Projection
import Eventful.ReadModel.Class
import Eventful.Store.Class

type EventBusHandler m serialized = StoredEvent serialized -> m ()

class (Monad m) => EventBus m bus serialized | bus -> serialized where
  publishEvent :: bus -> StoredEvent serialized -> m ()
  registerHandler :: bus -> EventBusHandler m serialized -> m ()
  registerStoreHandlerStart :: (EventStore m store serialized) => bus -> SequenceNumber -> store -> EventBusHandler m serialized -> m ()


eventBusRegisterStoreHandler
  :: (EventStore m store serialized, EventBus m bus serialized)
  => bus -> store -> EventBusHandler m serialized  -> m ()
eventBusRegisterStoreHandler bus = registerStoreHandlerStart bus 0

registerReadModel
  :: (ReadModel m model serialized, EventStore m store serialized, EventBus m bus serialized)
  => store -> bus -> model -> m ()
registerReadModel eventStore bus model = do
  seqNum <- latestApplied model
  let handler event = applyEvents model [event]
  registerStoreHandlerStart bus seqNum eventStore handler

storeAndPublishEvent
  :: ( MonadIO m
     , EventStore m store serializedes
     , EventBus m bus serializedeb
     , Serializable (Event proj) serializedes
     , Serializable (Event proj) serializedeb
     )
  => store -> bus -> AggregateId proj -> Event proj -> m ()
storeAndPublishEvent store bus uuid event = do
  storedEvents <- storeEvents store uuid [event]
  mapM_ (publishEvent bus) (serializeEvent <$> storedEvents)

-- TODO: This is not safe when multiple writers apply a command to the same
-- aggregate root (same UUID) at once. There is a race condition between
-- getting the projection and validating the command.
runAggregateCommand
  :: ( MonadIO m
     , Aggregate a
     , EventStore m store serializedes
     , EventBus m bus serializedeb
     , Serializable (Event a) serializedes
     , Serializable (Event a) serializedeb
     )
  => store -> bus -> AggregateId a -> Command a -> m (Maybe (CommandError a))
runAggregateCommand store bus uuid cmd = do
  proj <- getAggregate store uuid
  case command proj cmd of
    (Left err) -> return (Just err)
    (Right event) -> storeAndPublishEvent store bus uuid event >> return Nothing
