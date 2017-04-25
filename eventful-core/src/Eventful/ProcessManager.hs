module Eventful.ProcessManager
  ( ProcessManager (..)
  , ProcessManagerCommand (..)
  , ProcessManagerEvent (..)
  , ProcessManagerRouter (..)
  , processManagerHandler
  ) where

import Control.Monad (forM_, void)

import Eventful.Aggregate
import Eventful.Projection
import Eventful.Store.Class
import Eventful.UUID

-- | A 'ProcessManager' manages state between aggregates.
data ProcessManager state event command
  = ProcessManager
  { processManagerProjection :: Projection state event
  , processManagerPendingCommands :: state -> [ProcessManagerCommand event command]
  , processManagerPendingEvents :: state -> [ProcessManagerEvent event]
  }

data ProcessManagerCommand event command
  = forall state. ProcessManagerCommand
  { processManagerCommandAggregateId :: UUID
  , processManagerCommandAggregate :: Aggregate state event command
  , processManagerCommandCommand :: command
  }

instance (Show command, Show event) => Show (ProcessManagerCommand event command) where
  show (ProcessManagerCommand uuid _ command) =
    "ProcessManagerCommand{ processManagerCommandAggregateId = " ++ show uuid ++
    ", processManagerCommandCommand = " ++ show command

data ProcessManagerEvent event
  = ProcessManagerEvent
  { processManagerEventProjectionId :: UUID
  , processManagerEventEvent :: event
  } deriving (Show, Eq)

-- | A 'ProcessManagerRouter' decides which process manager projection ID to
-- use for a given event.
data ProcessManagerRouter state event command
  = ProcessManagerRouter
  { processManagerRouterGetManagerId :: UUID -> event -> Maybe UUID
    -- TODO: Should this really be just "Maybe UUID"? We should consider
    -- allowing UUID creation as well when the first event for a process comes
    -- in so we know every process manager has a unique UUID. We could make an
    -- ADT for ProcessManagerRouteResult that returns either the UUID, a value
    -- saying we need to make a UUID, or a value saying the manager ignores
    -- that event.
  , processManagerRouterManager :: ProcessManager state event command
  }

processManagerHandler
  :: (Monad m)
  => ProcessManagerRouter state event command
  -> EventStore event m
  -> UUID
  -> event
  -> m ()
processManagerHandler (ProcessManagerRouter getManagerId manager) store eventAggregateId event =
  maybe (return ()) (processManagerHandler' manager store event) (getManagerId eventAggregateId event)

processManagerHandler'
  :: (Monad m)
  => ProcessManager state event command
  -> EventStore event m
  -> event
  -> UUID
  -> m ()
processManagerHandler' ProcessManager{..} store startEvent managerId = do
  -- TODO: Don't ignore storage errors
  _ <- storeEvents store AnyVersion managerId [startEvent]
  (managerState, _) <- getLatestProjection store processManagerProjection managerId
  forM_ (processManagerPendingCommands managerState) $ \(ProcessManagerCommand aggregateId aggregate command) ->
    void $ commandStoredAggregate store aggregate aggregateId command
  forM_ (processManagerPendingEvents managerState) $ \(ProcessManagerEvent projectionId event) ->
    storeEvents store AnyVersion projectionId [event]
