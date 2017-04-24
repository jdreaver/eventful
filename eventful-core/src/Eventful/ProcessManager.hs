module Eventful.ProcessManager
  ( ProcessManager (..)
  ) where

import Eventful.Projection
import Eventful.UUID

-- | A 'ProcessManager' manages state between aggregates.
data ProcessManager state event command
  = ProcessManager
  { processManagerProjection :: Projection state event
  , processManagerPendingCommands :: state -> [(UUID, command)]
  , processManagerPendingEvents :: state -> [(UUID, event)]
  }
