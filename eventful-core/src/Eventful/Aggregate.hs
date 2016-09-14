-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  ) where

import Eventful.Projection

-- | An aggregate uses the information currently in a 'Projection' to produces
-- events from commands.
class (Projection a) => Aggregate a where
  data Command a :: *
  data CommandError a :: *
  command :: a -> Command a -> Either (CommandError a) (Event a)
