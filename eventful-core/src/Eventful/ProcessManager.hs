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

-- | A 'ProcessManager' manages interaction between aggregates. It works by
-- listening to events on an event bus (see 'processManagerHandler') and
-- applying events to its internal 'Projection'. Then, pending commands and
-- events are plucked off of that Projection and applied to the appropriate
-- Aggregates or Projections in other streams.
data ProcessManager state event command
  = ProcessManager
  { processManagerProjection :: Projection state event
  , processManagerPendingCommands :: state -> [ProcessManagerCommand event command]
  , processManagerPendingEvents :: state -> [ProcessManagerEvent event]
  }

-- | This is a @command@ along with the UUID of the target 'Aggregate', and
-- well as the 'Aggregate' instance. Note that this uses an existential type to
-- hide the @state@ type parameter on the Aggregate.
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

-- | This is an @event@ paired with the UUID of the stream to which the event
-- will be applied.
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

-- | This is an event handler for a 'ProcessManager' that applies all events to
-- the 'ProcessManagerRouter', and then applied any pending commands or events
-- to the appropriate places.
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
