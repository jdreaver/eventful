module EventSourcing.Aggregate
  ( Aggregate (..)
  ) where

import EventSourcing.Projection

-- | An aggregate uses the information currently in a 'Projection' to produces
-- events from commands.
class (Projection s) => Aggregate s where
  type Command s :: *
  type CommandError s :: *

  command :: s -> Command s -> Either (CommandError s) (Event s)
