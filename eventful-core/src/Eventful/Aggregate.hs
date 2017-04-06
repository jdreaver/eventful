-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  , allAggregateStates
  ) where

import Data.Foldable (foldl')
import Data.List (scanl')

import Eventful.Projection

-- | An 'Aggregate' is a combination of a 'Projection' and a function to
-- validate commands against that 'Projection'. When using an aggregate in some
-- service, it is common to simply load the latest projection state from the
-- event store and apply the command. If the command is valid then the new
-- event is applied to the projection in the event store.
data Aggregate state event cmd cmderror =
  Aggregate
  { aggregateCommand :: state -> cmd -> Either cmderror [event]
  , aggregateProjection :: Projection state event
  }

-- | Given a list commands, produce all of the states the aggregate's
-- projection sees, interspersed with command errors. This is useful for unit
-- testing aggregates.
allAggregateStates
  :: Aggregate state event cmd cmderror
  -> [cmd]
  -> [Either cmderror state]
allAggregateStates (Aggregate applyCommand (Projection seed apply)) events =
  map snd $ scanl' go (seed, Right seed) events
  where
    go (state, _) command =
      case applyCommand state command of
        Left err -> (state, Left err)
        Right outputEvents ->
          let state' = foldl' apply state outputEvents
          in (state', Right state')
