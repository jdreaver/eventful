module Eventful.EventBus.Pipes
  ( PipesEventBus (..)
  , pipesEventBus
  , module Eventful.EventBus.Class
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Pipes
import Pipes.Concurrent

import Eventful.EventBus.Class
import Eventful.Store

data PipesEventBus serialized =
  PipesEventBus
  { pipesEventBusQueues :: TVar [Output (StoredEvent serialized)]
  }

pipesEventBus :: IO (PipesEventBus serialized)
pipesEventBus = PipesEventBus <$> atomically (newTVar [])

instance EventBus IO (PipesEventBus serialized) serialized where
  publishEvent = publishEvent'
  registerStoreHandlerStart = registerStoreHandlerStart'

publishEvent' :: (MonadIO m) => PipesEventBus serialized -> StoredEvent serialized -> m ()
publishEvent' PipesEventBus{..} event =
  liftIO $ void $ atomically $ do
    queues <- readTVar pipesEventBusQueues
    mapM_ (`send` event) queues

-- TODO: Try to get this working in MonadIO, not IO. The problem lies in 'async'.
registerStoreHandlerStart'
  :: (EventStore IO store serialized)
  => PipesEventBus serialized -> SequenceNumber -> store -> EventBusHandler IO serialized -> IO ()
registerStoreHandlerStart' (PipesEventBus queuesTVar) seqNum store handler = do
  (output, input) <- liftIO $ spawn unbounded
  _ <- async $ do
    startPipe <- getSequencedEventsPipe store seqNum
    runEffect $ (startPipe >> fromInput input) >-> handlerConsumer handler
    performGC
  atomically $ modifyTVar' queuesTVar ((:) output)

handlerConsumer :: (Monad m) => EventBusHandler m serialized -> Consumer (StoredEvent serialized) m ()
handlerConsumer handler = forever $ await >>= lift . handler

-- TODO: Move this to a class so that individual event stores can decide to
-- make this more efficient.
getSequencedEventsPipe
  :: (EventStore m store serialized)
  => store -> SequenceNumber -> m (Producer (StoredEvent serialized) m ())
getSequencedEventsPipe store = fmap (mapM_ yield) . getSequencedEvents store
