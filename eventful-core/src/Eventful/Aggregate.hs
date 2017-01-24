-- | Defines an Aggregate type-class from DDD parlance.

module Eventful.Aggregate
  ( Aggregate (..)
  ) where

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

-- | An 'Aggregate' is a combination of a 'Projection' and a function to
-- validate 'Command's against that 'Projection'. When using an aggregate in
-- some service, it is common to simply load the latest projection state from
-- the event store and apply the command. If the command is valid then the new
-- event is applied to the projection in the event store.
data Aggregate proj event cmd cmderror =
  Aggregate
  { aggregateCommand :: proj -> cmd -> Either cmderror event
  }
