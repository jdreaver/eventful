module EventSourcing.EventBus
  ( Handler
  , EventBus (..)
  , newEventBus
  , publishEvent
  , runEventBus
  , startEventBus
  , storeAndPublishEvent
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class

import EventSourcing.Store
import EventSourcing.UUID

type Handler event m = UUID -> event -> m ()

data EventBus event m =
  EventBus
  { eventBusHandlers :: [Handler event m]
  , eventBusQueue :: TChan (UUID, event)
  }

newEventBus :: [Handler event m] -> IO (EventBus event m)
newEventBus handlers = do
  chan <- newTChanIO
  return $ EventBus handlers chan

publishEvent :: (MonadIO m) => EventBus event m -> UUID -> event -> m ()
publishEvent EventBus{..} uuid event = liftIO . atomically $ writeTChan eventBusQueue (uuid, event)

runEventBus :: (MonadIO m) => EventBus event m -> m ()
runEventBus EventBus{..} = forever $ do
  (uuid, event) <- liftIO $ atomically $ readTChan eventBusQueue
  mapM_ (\handler -> handler uuid event) eventBusHandlers

startEventBus :: EventBus event IO -> IO ()
startEventBus bus = void . forkIO $ runEventBus bus

storeAndPublishEvent
  :: (MonadIO m, EventStore store m event) => store -> EventBus event m -> UUID -> event -> m ()
storeAndPublishEvent store bus uuid event =
  storeEvents store uuid [event] >> publishEvent bus uuid event
