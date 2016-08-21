module EventSourcing.EventBus
  ( Handler
  , EventBus (..)
  , newEventBus
  , publishEvent
  , storeAndPublishEvent
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import EventSourcing.Store
import EventSourcing.UUID

type Handler event m = UUID -> event -> m ()

data EventBus event =
  EventBus
  { eventBusQueues :: [TChan (UUID, event)]
  }

newEventBus :: [Handler event IO] -> IO (EventBus event)
newEventBus handlers = do
  tchans <- forM handlers $ \handler -> do
    chan <- newTChanIO
    void . forkIO $ runHandler handler chan
    return chan
  return $ EventBus tchans

runHandler :: Handler event IO -> TChan (UUID, event) -> IO ()
runHandler handler chan = forever $ do
  (uuid, event) <- liftIO $ atomically $ readTChan chan
  handler uuid event

publishEvent :: (MonadIO m) => EventBus event -> UUID -> event -> m ()
publishEvent EventBus{..} uuid event =
  liftIO $ forM_ eventBusQueues $ \chan -> atomically $ writeTChan chan (uuid, event)

storeAndPublishEvent
  :: (MonadIO m, EventStore store m event) => store -> EventBus event -> UUID -> event -> m ()
storeAndPublishEvent store bus uuid event =
  storeEvents store uuid [event] >> publishEvent bus uuid event
