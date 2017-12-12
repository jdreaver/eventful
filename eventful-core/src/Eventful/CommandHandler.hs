{-# LANGUAGE RecordWildCards #-}

-- | Defines a Command Handler type.

module Eventful.CommandHandler
  ( CommandHandler (..)
  , allCommandHandlerStates
  , applyCommandHandler
  , serializedCommandHandler
  ) where

import Data.Foldable (foldl')
import Data.List (scanl')

import Eventful.Projection
import Eventful.Serializer
import Eventful.Store.Class
import Eventful.UUID

-- | An 'CommandHandler' is a combination of a 'Projection' and a function to
-- validate commands against that 'Projection'. When using a command handler in
-- some service, it is common to simply load the latest projection state from
-- the event store and handle the command. If the command is valid then the new
-- events are applied to the projection in the event store.
data CommandHandler state event command =
  CommandHandler
  { commandHandlerHandler :: state -> command -> [event]
  , commandHandlerProjection :: Projection state event
  }

-- | Given a list commands, produce all of the states the command handler's
-- projection sees. This is useful for unit testing a 'CommandHandler'.
allCommandHandlerStates
  :: CommandHandler state event command
  -> [command]
  -> [state]
allCommandHandlerStates (CommandHandler commandHandler (Projection seed eventHandler)) events =
  scanl' go seed events
  where
    go state command = foldl' eventHandler state $ commandHandler state command

-- | Loads the latest version of a 'Projection' from the event store and tries to
-- apply the 'CommandHandler' command to it. If the command succeeds, then this
-- saves the events back to the store as well.
applyCommandHandler
  :: (Monad m)
  => VersionedEventStoreWriter m event
  -> VersionedEventStoreReader m event
  -> CommandHandler state event command
  -> UUID
  -> command
  -> m [event]
applyCommandHandler writer reader (CommandHandler handler proj) uuid command = do
  StreamProjection{..} <- getLatestStreamProjection reader (versionedStreamProjection uuid proj)
  let events = handler streamProjectionState command
  mError <- storeEvents writer uuid (ExactPosition streamProjectionPosition) events
  case mError of
    Left err -> error $ "TODO: Create CommandHandler restart logic. " ++ show err
    Right _ -> return events

-- | Use a pair of 'Serializer's to wrap a 'CommandHandler' with event type @event@
-- and command type @command@ so it uses the @serializedEvent@ and
-- @serializedCommand@ types.
serializedCommandHandler
  :: CommandHandler state event command
  -> Serializer event serializedEvent
  -> Serializer command serializedCommand
  -> CommandHandler state serializedEvent serializedCommand
serializedCommandHandler (CommandHandler commandHandler projection) eventSerializer commandSerializer =
  CommandHandler serializedHandler serializedProjection'
  where
    serializedProjection' = serializedProjection projection eventSerializer
    -- Try to deserialize the command and apply the handler. If we can't
    -- deserialize, then just return no events. We also need to serialize the
    -- events after of course.
    serializedHandler state = map (serialize eventSerializer) . maybe [] (commandHandler state) . deserialize commandSerializer
