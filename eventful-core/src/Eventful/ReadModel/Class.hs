{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.ReadModel.Class
  ( ReadModel (..)
  , runPollingReadModel
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Eventful.Store.Class

data ReadModel model serialized m =
  ReadModel
  { readModelModel :: model
  , readModelLatestAppliedSequence :: model -> m SequenceNumber
  , readModelHandleEvents :: model -> [GlobalStreamEvent serialized] -> m ()
  }

type PollingPeriodSeconds = Double

runPollingReadModel
  :: (MonadIO m, Monad mstore)
  => ReadModel model serialized m
  -> GlobalEventStoreReader mstore serialized
  -> (forall a. mstore a -> m a)
  -> PollingPeriodSeconds
  -> m ()
runPollingReadModel ReadModel{..} globalReader runStore waitSeconds = forever $ do
  -- Get new events starting from latest applied sequence number
  latestSeq <- readModelLatestAppliedSequence readModelModel
  newEvents <- runStore $ getEvents globalReader (eventsStartingAt () $ latestSeq + 1)

  -- Handle the new events
  readModelHandleEvents readModelModel newEvents

  -- Wait before running again
  liftIO $ threadDelay $ ceiling (waitSeconds * 1000000) -- threadDelay accepts microseconds

-- data EventHandler m serialized = forall event. (Serializable event serialized, Monad m) => EventHandler (event -> m ())

-- combineHandlers :: (Monad m) => [EventHandler m serialized] -> (serialized -> m ())
-- combineHandlers handlers event = mapM_ ($ event) (mkHandler <$> handlers)

-- mkHandler :: EventHandler m serialized -> (serialized -> m ())
-- mkHandler (EventHandler handler) event = maybe (return ()) handler (deserialize event)
