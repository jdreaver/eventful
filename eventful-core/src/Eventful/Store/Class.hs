module Eventful.Store.Class
  ( EventStore (..)
  , getEvents
  , storeEvents
  , ProjectionId (..)
  , StoredEvent (..)
  , serializeEvent
  , deserializeEvent
  , EventVersion (..)
  , SequenceNumber (..)
  ) where

import Data.Aeson
import Data.Maybe (mapMaybe)
import Web.HttpApiData
import Web.PathPieces

import Eventful.Projection
import Eventful.Serializable
import Eventful.UUID

class (Monad m) => EventStore m store serialized | store -> serialized where
  getAllUuids :: store -> m [UUID]
  getEventsRaw :: store -> UUID -> m [StoredEvent serialized]
  storeEventsRaw :: store -> UUID -> [serialized] -> m [StoredEvent serialized]
  latestEventVersion :: store -> UUID -> m EventVersion

  -- Some implementations might have a more efficient ways to do the this
  getAggregate :: (Projection proj, Serializable (Event proj) serialized) => store -> ProjectionId proj -> m proj
  getAggregate store uuid = latestProjection . fmap storedEventEvent <$> getEvents store uuid

  getSequencedEvents :: store -> SequenceNumber -> m [StoredEvent serialized]

getEvents
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> ProjectionId proj -> m [StoredEvent (Event proj)]
getEvents store (ProjectionId uuid) = mapMaybe deserialize <$> getEventsRaw store uuid

storeEvents
  :: (Serializable (Event proj) serialized, EventStore m store serialized)
  => store -> ProjectionId proj -> [Event proj] -> m [StoredEvent (Event proj)]
storeEvents store (ProjectionId uuid) events = do
  serialized <- storeEventsRaw store uuid (serialize <$> events)
  return $ zipWith (<$) events serialized

-- | This type ensures our stored events have the correct type, but it also
-- allows us to avoid type ambiguity errors in event stores by providing the
-- phantom projection type.
newtype ProjectionId proj = ProjectionId { unProjectionId :: UUID }
  deriving (Show, Eq, Ord, ToJSON, FromJSON, FromHttpApiData)

data StoredEvent event
  = StoredEvent
  { storedEventProjectionId :: UUID
  , storedEventVersion :: EventVersion
  , storedEventSequenceNumber :: SequenceNumber
  , storedEventEvent :: event
  } deriving (Show, Eq, Functor)

instance (Serializable a b) => Serializable (StoredEvent a) (StoredEvent b) where
  serialize = serializeEvent
  deserialize = deserializeEvent

serializeEvent :: (Serializable event serialized) => StoredEvent event -> StoredEvent serialized
serializeEvent = fmap serialize

deserializeEvent :: (Serializable event serialized) => StoredEvent serialized -> Maybe (StoredEvent event)
deserializeEvent (StoredEvent uuid vers seqNum event) =
  StoredEvent uuid vers seqNum <$> deserialize event

newtype EventVersion = EventVersion { unEventVersion :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON)

newtype SequenceNumber = SequenceNumber { unSequenceNumber :: Int }
  deriving (Show, Read, Ord, Eq, Enum, Num, FromJSON, ToJSON,
            PathPiece, ToHttpApiData, FromHttpApiData)
