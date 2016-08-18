module EventSourcing.Store
  ( EventStore (..)
  ) where

import EventSourcing.UUID

-- Is the UUID part really necessary? Should that be handled in a StoredEvent
-- type?
class (Monad m) => EventStore store m event | store -> event where
  getUuids :: store -> m [UUID]
  getEvents :: store -> UUID -> m [event]
  storeEvents :: store -> UUID -> [event] -> m ()


-- data StoredEvent event
--   = StoredEvent
--   { storedEventEvent :: event
--   , storedEventSequenceNumber :: SequenceNumber
--   , storedEventTime :: UTCTime
--   }

-- newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
--   deriving (Show, Ord, Eq)
