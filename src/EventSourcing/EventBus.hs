module EventSourcing.EventBus
  ( Handler
  , EventBus (..)
  , newEventBus
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
  { eventBusQueues :: Output (UUID, event)
  }

newEventBus :: [Handler event IO] -> IO (EventBus event)
newEventBus handlers = do
  outputs <- forM handlers $ \handler -> do
    (output, input) <- spawn unbounded
    _ <- async $ do
      runEffect $ fromInput input >-> handlerConsumer handler
      performGC
    return output
  return $ EventBus (mconcat outputs)

handlerConsumer :: (Monad m) => Handler event m -> Consumer (UUID, event) m ()
handlerConsumer handler = forever $ do
  (uuid, event) <- await
  lift $ handler uuid event

-- runHandler :: Handler event IO -> TChan (UUID, event) -> IO ()
-- runHandler handler chan = do
--   (uuid, event) <- liftIO $ atomically $ readTChan chan
--   handler uuid event

publishEvent :: (MonadIO m) => EventBus event -> UUID -> event -> m ()
publishEvent EventBus{..} uuid event =
  liftIO $ void $ atomically $ send eventBusQueues (uuid, event)

storeAndPublishEvent
  :: (MonadIO m, EventStore store m event) => store -> EventBus event -> UUID -> event -> m ()
storeAndPublishEvent store bus uuid event =
  storeEvents store uuid [event] >> publishEvent bus uuid event
