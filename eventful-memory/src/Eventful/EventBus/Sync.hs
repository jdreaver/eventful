module Eventful.EventBus.Sync
  ( SyncEventBusIO
  , syncEventBusIO
  , module Eventful.EventBus.Class
  ) where

import Control.Monad.IO.Class
import Data.IORef

import Eventful.EventBus.Class
import Eventful.Store

newtype SyncEventBusIO m serialized = SyncEventBusIO (IORef [EventBusHandler m serialized])

syncEventBusIO :: IO (SyncEventBusIO m serialized)
syncEventBusIO = SyncEventBusIO <$> newIORef []

instance (MonadIO m) => EventBus m (SyncEventBusIO m serialized) serialized where
  publishEvent (SyncEventBusIO handlersRef) event = do
    handlers <- liftIO $ readIORef handlersRef
    mapM_ ($ event) handlers

  registerStoreHandlerStart (SyncEventBusIO handlersRef) seqNum store handler = do
    startEvents <- getSequencedEvents store seqNum
    mapM_ handler startEvents
    liftIO $ modifyIORef handlersRef (\handlers -> handler : handlers)
