-- | Defines an Aggregate type-class from DDD parlance.

module EventSourcing.Aggregate
  ( Aggregate (..)
  ) where

import EventSourcing.Projection

-- | An aggregate uses the information currently in a 'Projection' to produces
-- events from commands.
class (Projection a) => Aggregate a where
  data Command a :: *
  data CommandError a :: *
  command :: a -> Command a -> Either (CommandError a) (Event a)


-- TODO: This is not safe when multiple writers apply a command to the same
-- aggregate root (same UUID) at once. There is a race condition between
-- getting the projection and validating the command.
-- eventStoreCommand
--   :: (MonadIO m, EventStore store m storage, Aggregate a, Serializable (Event a) storage)
--   => store -> EventBus (Event a) -> UUID -> Command a -> m (Maybe (CommandError a))
-- eventStoreCommand store bus uuid cmd = do
--   proj <- getLatestProjection store uuid
--   case command proj cmd of
--     (Left err) -> return (Just err)
--     (Right event) -> storeAndPublishEvent store bus uuid event >> return Nothing
