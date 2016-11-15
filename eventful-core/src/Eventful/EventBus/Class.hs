module Eventful.EventBus.Class
  ( EventBus (..)
  , EventBusHandler
  , registerStoreHandler
  , registerReadModel
  , storeAndPublishEvents
  , runAggregateCommand
  ) where

import Eventful.Aggregate
import Eventful.Projection
import Eventful.ReadModel.Class
import Eventful.Serializable
import Eventful.Store.Class
import Eventful.UUID

type EventBusHandler m serialized = StoredEvent serialized -> m ()

class (Monad m) => EventBus m bus serialized | bus -> serialized where
  publishEvent :: bus -> StoredEvent serialized -> m ()
  registerStoreHandlerStart :: (EventStore m store serialized) => bus -> SequenceNumber -> store -> EventBusHandler m serialized -> m ()


registerStoreHandler
  :: (EventStore m store serialized, EventBus m bus serialized)
  => bus -> store -> EventBusHandler m serialized  -> m ()
registerStoreHandler bus = registerStoreHandlerStart bus 0

registerReadModel
  :: (ReadModel m model serialized, EventStore m store serialized, EventBus m bus serialized)
  => store -> bus -> model -> m ()
registerReadModel eventStore bus model = do
  seqNum <- latestApplied model
  let handler event = applyEvents model [event]
  registerStoreHandlerStart bus seqNum eventStore handler

storeAndPublishEvents
  :: ( EventStore m store serializedes
     , EventBus m bus serializedeb
     , Serializable event serializedes
     , Serializable event serializedeb
     )
  => store -> bus -> UUID -> [event] -> m ()
storeAndPublishEvents store bus uuid events = do
  storedEvents <- storeEvents store uuid events
  mapM_ (publishEvent bus) (serialize <$> storedEvents)

-- TODO: This is not safe when multiple writers apply a command to the same
-- aggregate root (same UUID) at once. There is a race condition between
-- getting the projection and validating the command.
runAggregateCommand
  :: ( Aggregate proj
     , EventStore m store serializedes
     , EventBus m bus serializedeb
     , Serializable proj serializedes
     , Serializable (Event proj) serializedes
     , Serializable (Event proj) serializedeb
     )
  => store -> bus -> UUID -> Command proj -> m (proj, Maybe (CommandError proj))
runAggregateCommand store bus uuid cmd = do
  proj <- getLatestProjection store uuid
  case command proj cmd of
    (Left err) -> return (proj, Just err)
    (Right event) -> storeAndPublishEvents store bus uuid [event] >> return (proj, Nothing)
