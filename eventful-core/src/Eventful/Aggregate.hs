-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  , allAggregateStates
  , commandStoredAggregate
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
-- event store and apply the command. If the command is valid then the new
-- event is applied to the projection in the event store.
data Aggregate state event cmd =
  Aggregate
  { aggregateCommand :: state -> cmd -> [event]
  , aggregateProjection :: Projection state event
  }

-- | Given a list commands, produce all of the states the aggregate's
-- projection sees, interspersed with command errors. This is useful for unit
-- testing aggregates.
allAggregateStates
  :: Aggregate state event cmd
  -> [cmd]
  -> [state]
allAggregateStates (Aggregate applyCommand (Projection seed apply)) events =
  scanl' go seed events
  where
    go state command = foldl' apply state $ applyCommand state command

-- | Loads the latest version of a projection from the event store and tries to
-- apply the 'Aggregate' command to it. If the command succeeds, then this
-- saves the events back to the store as well.
commandStoredAggregate
  :: (Monad m)
  => EventStore serialized m
  -> Serializer event serialized
  -> Aggregate state event cmd
  -> UUID
  -> cmd
  -> m [event]
commandStoredAggregate store serializer@Serializer{..} (Aggregate applyCommand proj) uuid command = do
  (latest, vers) <- getLatestProjection store serializer proj uuid
  let events = applyCommand latest command
  mError <- storeEvents store (ExactVersion vers) uuid (serialize <$> events)
  case mError of
    (Just err) -> error $ "TODO: Create aggregate restart logic. " ++ show err
    Nothing -> return events
