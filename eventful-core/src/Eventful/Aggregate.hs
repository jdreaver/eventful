-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  ) where

import Eventful.Projection

-- | An 'Aggregate' is a combination of a 'Projection' and a function to
-- validate 'Command's against that 'Projection'. When using an aggregate in
-- some service, it is common to simply load the latest projection state from
-- the event store and apply the command. If the command is valid then the new
-- event is applied to the projection in the event store.
class (Projection a) => Aggregate a where
  data Command a :: *
  data CommandError a :: *
  command :: a -> Command a -> Either (CommandError a) (Event a)
