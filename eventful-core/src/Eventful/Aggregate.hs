{-# LANGUAGE RecordWildCards #-}

-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  , allAggregateStates
  , commandStoredAggregate
  , serializedAggregate
  ) where

import Data.Foldable (foldl')
import Data.List (scanl')

import Eventful.Projection
import Eventful.Serializer
import Eventful.Store.Class
import Eventful.UUID

-- | An 'Aggregate' is a combination of a 'Projection' and a function to
-- validate commands against that 'Projection'. When using an aggregate in some
-- service, it is common to simply load the latest projection state from the
-- event store and handle the command. If the command is valid then the new
-- events are applied to the projection in the event store.
data Aggregate state event command =
  Aggregate
  { aggregateCommandHandler :: state -> command -> [event]
  , aggregateProjection :: Projection state event
  }

-- | Given a list commands, produce all of the states the aggregate's
-- projection sees. This is useful for unit testing aggregates.
allAggregateStates
  :: Aggregate state event command
  -> [command]
  -> [state]
allAggregateStates (Aggregate commandHandler (Projection seed eventHandler)) events =
  scanl' go seed events
  where
    go state command = foldl' eventHandler state $ commandHandler state command

-- | Loads the latest version of a 'Projection' from the event store and tries to
-- apply the 'Aggregate' command to it. If the command succeeds, then this
-- saves the events back to the store as well.
commandStoredAggregate
  :: (Monad m)
  => EventStore serialized m
  -> Aggregate state serialized command
  -> UUID
  -> command
  -> m [serialized]
commandStoredAggregate store (Aggregate handler proj) uuid command = do
  StreamProjection{..} <- getLatestVersionedProjection store (versionedStreamProjection uuid proj)
  let events = handler streamProjectionState command
  mError <- storeEvents store (ExactVersion streamProjectionPosition) uuid events
  case mError of
    (Just err) -> error $ "TODO: Create aggregate restart logic. " ++ show err
    Nothing -> return events

-- | Use a pair of 'Serializer's to wrap a 'Aggregate' with event type @event@
-- and command type @command@ so it uses the @serializedEvent@ and
-- @serializedCommand@ types.
serializedAggregate
  :: Aggregate state event command
  -> Serializer event serializedEvent
  -> Serializer command serializedCommand
  -> Aggregate state serializedEvent serializedCommand
serializedAggregate (Aggregate commandHandler projection) eventSerializer commandSerializer =
  Aggregate serializedHandler serializedProjection'
  where
    serializedProjection' = serializedProjection projection eventSerializer
    -- Try to deserialize the command and apply the handler. If we can't
    -- deserialize, then just return no events. We also need to serialize the
    -- events after of course.
    serializedHandler state = map (serialize eventSerializer) . maybe [] (commandHandler state) . deserialize commandSerializer
