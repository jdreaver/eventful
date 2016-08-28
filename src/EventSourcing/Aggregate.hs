-- | Defines an Aggregate type-class from DDD parlance.

module EventSourcing.Aggregate
  ( Aggregate (..)
  , eventStoreCommand
  ) where

import Control.Monad.IO.Class

import EventSourcing.EventBus
import EventSourcing.Projection
import EventSourcing.Store


-- | An aggregate uses the information currently in a 'Projection' to produces
-- events from commands.
class (Projection a) => Aggregate a where
  data Command a :: *
  data CommandError a :: *
  command :: a -> Command a -> Either (CommandError a) (Event a)


-- TODO: This is not safe when multiple writers apply a command to the same
-- aggregate root (same UUID) at once. There is a race condition between
-- getting the projection and validating the command.
eventStoreCommand
  :: (MonadIO m, EventStore m store a, Aggregate a)
  => store -> (Event a -> serialized) -> EventBus serialized -> AggregateId a -> Command a -> m (Maybe (CommandError a))
eventStoreCommand store serialize bus uuid cmd = do
  proj <- getAggregate store uuid
  case command proj cmd of
    (Left err) -> return (Just err)
    (Right event) -> storeAndPublishEvent store serialize bus uuid event >> return Nothing
