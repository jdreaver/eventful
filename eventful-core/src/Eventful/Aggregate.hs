-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  , allAggregateStates
  , commandStoredAggregate
  ) where

import Data.Foldable (foldl')
import Data.List (scanl')

import Eventful.Projection
import Eventful.Store.Class
import Eventful.UUID

-- | An 'Aggregate' is a combination of a 'Projection' and a function to
-- validate commands against that 'Projection'. When using an aggregate in some
-- service, it is common to simply load the latest projection state from the
-- event store and handle the command. If the command is valid then the new
-- event is applied to the projection in the event store.
data Aggregate state event cmd =
  Aggregate
  { aggregateCommandHandler :: state -> cmd -> [event]
  , aggregateProjection :: Projection state event
  }

-- | Given a list commands, produce all of the states the aggregate's
-- projection sees, interspersed with command errors. This is useful for unit
-- testing aggregates.
allAggregateStates
  :: Aggregate state event cmd
  -> [cmd]
  -> [state]
allAggregateStates (Aggregate commandHandler (Projection seed eventHandler)) events =
  scanl' go seed events
  where
    go state command = foldl' eventHandler state $ commandHandler state command

-- | Loads the latest version of a projection from the event store and tries to
-- apply the 'Aggregate' command to it. If the command succeeds, then this
-- saves the events back to the store as well.
commandStoredAggregate
  :: (Monad m)
  => EventStore serialized m
  -> Aggregate state serialized cmd
  -> UUID
  -> cmd
  -> m [serialized]
commandStoredAggregate store (Aggregate handler proj) uuid command = do
  (latest, vers) <- getLatestProjection store proj uuid
  let events = handler latest command
  mError <- storeEvents store (ExactVersion vers) uuid events
  case mError of
    (Just err) -> error $ "TODO: Create aggregate restart logic. " ++ show err
    Nothing -> return events
