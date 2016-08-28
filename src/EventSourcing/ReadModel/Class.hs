module EventSourcing.ReadModel.Class
  ( ProjectionReadModel (..)
  ) where

import EventSourcing.Projection
import EventSourcing.Store
import EventSourcing.UUID

class (Projection proj, Monad m) => ProjectionReadModel m store proj | store -> proj where
  latestApplied :: store -> m SequenceNumber
  getProjection :: store -> UUID -> m proj
  applyEvents :: store -> [StoredEvent (Event proj)] -> m ()

-- data StoredProjection proj
--   = StoredProjection
--   { storedProjectionProjection :: proj
--   , storedProjectionEventVersion :: EventVersion
--   } deriving (Show)
