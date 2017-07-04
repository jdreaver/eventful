module Eventful.ReadModel.Memory
  ( memoryReadModel
  ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Safe (maximumDef)

import Eventful.ReadModel.Class
import Eventful.Store.Class

data MemoryReadModelData modeldata
  = MemoryReadModelData
  { memoryReadModelDataLatestSequenceNumber :: SequenceNumber
  , _memoryReadModelDataValue :: modeldata
  } deriving (Show)

-- | Creates a read model that wraps some pure data in a TVar and manages the
-- latest sequence number for you.
memoryReadModel
  :: (MonadIO m)
  => modeldata
  -> (modeldata -> [GlobalStreamEvent serialized] -> m modeldata)
  -> IO (ReadModel (TVar (MemoryReadModelData modeldata)) serialized m)
memoryReadModel initialValue handleEvents = do
  tvar <- newTVarIO $ MemoryReadModelData (-1) initialValue
  return $ ReadModel tvar getLatestSequence handleTVarEvents
  where
    getLatestSequence tvar' = liftIO $ memoryReadModelDataLatestSequenceNumber <$> readTVarIO tvar'
    handleTVarEvents tvar' events = do
      (MemoryReadModelData latestSeq modelData) <- liftIO $ readTVarIO tvar'
      let latestSeq' = maximumDef latestSeq (streamEventPosition <$> events)
      modelData' <- handleEvents modelData events
      liftIO . atomically . writeTVar tvar' $ MemoryReadModelData latestSeq' modelData'
