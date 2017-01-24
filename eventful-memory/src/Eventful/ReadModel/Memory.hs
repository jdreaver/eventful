module Eventful.ReadModel.Memory
  ( MemoryReadModel (..)
  , memoryReadModel
  , projectionMemoryReadModel
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Eventful.Projection
import Eventful.ProjectionMap
import Eventful.ReadModel.Class
import Eventful.Serializable
import Eventful.Store

data MemoryReadModel m serialized
  = MemoryReadModel
  { memoryReadModelLatestApplied :: TVar SequenceNumber
  , memoryReadModelHandler :: StoredEvent serialized -> m ()
  }

instance (MonadIO m) => ReadModel m (MemoryReadModel m serialized) serialized where
  latestApplied (MemoryReadModel appliedTVar _) = liftIO $ readTVarIO appliedTVar
  applyEvents (MemoryReadModel appliedTVar handler) events = do
    liftIO $ atomically $ modifyTVar appliedTVar $ \currentMax -> maximum (currentMax : (storedEventSequenceNumber <$> events))
    mapM_ handler events

memoryReadModel :: (Monad m) => [EventHandler m (StoredEvent serialized)] -> IO (MemoryReadModel m serialized)
memoryReadModel handlers =
  MemoryReadModel <$> newTVarIO 0 <*> return (combineHandlers handlers)


projectionMemoryReadModel
  :: (MonadIO m, Serializable event serialized)
  => Projection proj event -> IO (MemoryReadModel m serialized, TVar (ProjectionMap proj))
projectionMemoryReadModel proj = do
  tvar <- newTVarIO projectionMap
  let handler (StoredEvent uuid _ _ event) = liftIO $ atomically $ modifyTVar' tvar (applyProjectionMap proj uuid event)
  model <- memoryReadModel [EventHandler handler]
  return (model, tvar)
