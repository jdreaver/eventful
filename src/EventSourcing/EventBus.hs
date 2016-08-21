module EventSourcing.EventBus
  ( Handler
  , EventBus (..)
  , eventBus
  , registerHandler
  , publishEvent
  , storeAndPublishEvent
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Pipes
import Pipes.Concurrent

import EventSourcing.Store
import EventSourcing.UUID

type Handler event m = UUID -> event -> m ()

data EventBus event =
  EventBus
  { eventBusQueues :: TVar [Output (UUID, event)]
  }

eventBus :: IO (EventBus event)
eventBus = EventBus <$> atomically (newTVar [])

-- TODO: Pass event store here too and hydrate pipe with previous events
registerHandler :: EventBus event -> Handler event IO -> IO ()
registerHandler (EventBus queuesTVar) handler = do
  (output, input) <- spawn unbounded
  _ <- async $ do
    runEffect $ fromInput input >-> handlerConsumer handler
    performGC
  atomically $ modifyTVar' queuesTVar ((:) output)

handlerConsumer :: (Monad m) => Handler event m -> Consumer (UUID, event) m ()
handlerConsumer handler = forever $ do
  (uuid, event) <- await
  lift $ handler uuid event


publishEvent :: (MonadIO m) => EventBus event -> UUID -> event -> m ()
publishEvent EventBus{..} uuid event =
  liftIO $ void $ atomically $ do
    queues <- readTVar eventBusQueues
    mapM_ (`send` (uuid, event)) queues

storeAndPublishEvent
  :: (MonadIO m, EventStore store m event) => store -> EventBus event -> UUID -> event -> m ()
storeAndPublishEvent store bus uuid event =
  storeEvents store uuid [event] >> publishEvent bus uuid event
