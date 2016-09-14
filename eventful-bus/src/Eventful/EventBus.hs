module Eventful.EventBus
  ( Handler
  , EventBus (..)
  , eventBus
  , registerHandler
  , registerReadModel
  , publishEvent
  , storeAndPublishEvent
  , eventStoreCommand
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Pipes
import Pipes.Concurrent

import Eventful.Aggregate
import Eventful.ReadModel
import Eventful.Projection
import Eventful.Store

type Handler serialized m = StoredEvent serialized -> m ()

data EventBus serialized =
  EventBus
  { eventBusQueues :: TVar [Output (StoredEvent serialized)]
  }

eventBus :: IO (EventBus event)
eventBus = EventBus <$> atomically (newTVar [])

registerHandler
  :: (EventStore IO store serialized)
  => store -> EventBus serialized -> Handler serialized IO -> IO ()
registerHandler = registerHandlerStart 0

registerHandlerStart
  :: (EventStore IO store serialized)
  => SequenceNumber -> store -> EventBus serialized -> Handler serialized IO -> IO ()
registerHandlerStart seqNum store (EventBus queuesTVar) handler = do
  (output, input) <- spawn unbounded
  _ <- async $ do
    startPipe <- getSequencedEventsPipe store seqNum
    runEffect $ (startPipe >> fromInput input) >-> handlerConsumer handler
    performGC
  atomically $ modifyTVar' queuesTVar ((:) output)

registerReadModel
  :: (ReadModel IO model serialized, EventStore IO store serialized)
  => store -> EventBus serialized -> model -> IO ()
registerReadModel eventStore bus model = do
  seqNum <- latestApplied model
  let handler event = applyEvents model [event]
  registerHandlerStart seqNum eventStore bus handler

handlerConsumer :: (Monad m) => Handler serialized m -> Consumer (StoredEvent serialized) m ()
handlerConsumer handler = forever $ await >>= lift . handler

publishEvent :: (MonadIO m) => EventBus serialized -> StoredEvent serialized -> m ()
publishEvent EventBus{..} event =
  liftIO $ void $ atomically $ do
    queues <- readTVar eventBusQueues
    mapM_ (`send` event) queues

storeAndPublishEvent
  :: ( MonadIO m
     , EventStore m store serializedes
     , Serializable (Event proj) serializedes
     , Serializable (Event proj) serializedeb
     )
  => store -> EventBus serializedeb -> AggregateId proj -> Event proj -> m ()
storeAndPublishEvent store bus uuid event = do
  storedEvents <- storeEvents store uuid [event]
  mapM_ (publishEvent bus) (serializeEvent <$> storedEvents)

-- TODO: Move this to a class so that individual event stores can decide to
-- make this more efficient.
getSequencedEventsPipe
  :: (EventStore m store serialized)
  => store -> SequenceNumber -> m (Producer (StoredEvent serialized) m ())
getSequencedEventsPipe store = fmap (mapM_ yield) . getSequencedEvents store


-- TODO: This is not safe when multiple writers apply a command to the same
-- aggregate root (same UUID) at once. There is a race condition between
-- getting the projection and validating the command.
eventStoreCommand
  :: ( MonadIO m
     , Aggregate a
     , EventStore m store serializedes
     , Serializable (Event a) serializedes
     , Serializable (Event a) serializedeb
     )
  => store -> EventBus serializedeb -> AggregateId a -> Command a -> m (Maybe (CommandError a))
eventStoreCommand store bus uuid cmd = do
  proj <- getAggregate store uuid
  case command proj cmd of
    (Left err) -> return (Just err)
    (Right event) -> storeAndPublishEvent store bus uuid event >> return Nothing
