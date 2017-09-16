{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module Eventful.ProcessManager
  ( ProcessManager (..)
  , ProcessManagerCommand (..)
  , applyProcessManagerCommandsAndEvents
  ) where

import Control.Monad (forM_, void)

import Eventful.CommandHandler
import Eventful.Projection
import Eventful.Store.Class
import Eventful.UUID

-- | A 'ProcessManager' manages interaction between event streams. It works by
-- listening to events on an event bus and applying events to its internal
-- 'Projection' (see 'applyProcessManagerCommandsAndEvents'). Then, pending
-- commands and events are plucked off of that Projection and applied to the
-- appropriate 'CommandHandler' or Projections in other streams.
data ProcessManager state event command
  = ProcessManager
  { processManagerProjection :: Projection state (VersionedStreamEvent event)
  , processManagerPendingCommands :: state -> [ProcessManagerCommand event command]
  , processManagerPendingEvents :: state -> [StreamEvent UUID () event]
  }

-- | This is a @command@ along with the UUID of the target 'CommandHandler', as
-- well as the 'CommandHandler' type. Note that this uses an existential type
-- to hide the @state@ type parameter on the CommandHandler.
data ProcessManagerCommand event command
  = forall state. ProcessManagerCommand
  { processManagerCommandTargetId :: UUID
  , processManagerCommandCommandHandler :: CommandHandler state event command
  , processManagerCommandCommand :: command
  }

instance (Show command, Show event) => Show (ProcessManagerCommand event command) where
  show (ProcessManagerCommand uuid _ command) =
    "ProcessManagerCommand{processManagerCommandCommandHandlerId = " ++ show uuid ++
    ", processManagerCommandCommand = " ++ show command ++ "}"

-- | Plucks the pending commands and events off of the process manager's state
-- and applies them to the appropriate locations in the event store.
applyProcessManagerCommandsAndEvents
  :: (Monad m)
  => ProcessManager state event command
  -> VersionedEventStoreWriter m event
  -> VersionedEventStoreReader m event
  -> state
  -> m ()
applyProcessManagerCommandsAndEvents ProcessManager{..} writer reader state = do
  forM_ (processManagerPendingCommands state) $ \(ProcessManagerCommand targetId commandHandler command) ->
    void $ applyCommandHandler writer reader commandHandler targetId command
  forM_ (processManagerPendingEvents state) $ \(StreamEvent projectionId () event) ->
    storeEvents writer projectionId AnyPosition [event]
