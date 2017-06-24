{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Eventful.Store.Queries
  ( QueryRange (..)
  , QueryStart (..)
  , QueryLimit (..)
  , allEvents
  , eventsUntil
  , eventsStartingAt
  , eventsStartingAtUntil
  , eventsStartingAtTakeLimit
  , StreamEvent (..)
  ) where

-- | This type defines how to query an event stream. It defines the stream key
-- and the start/stop points for the query.
data QueryRange key orderKey
  = QueryRange
  { queryRangeKey :: key
  , queryRangeStart :: QueryStart orderKey
  , queryRangeLimit :: QueryLimit orderKey
  } deriving (Show, Eq)

-- | This type defines where an event store query starts.
data QueryStart orderKey
  = StartFromBeginning
  | StartQueryAt orderKey
  deriving (Show, Eq, Functor)

-- | This type is used to limit the results of a query from an event store.
data QueryLimit orderKey
  = NoQueryLimit
  | MaxNumberOfEvents Int
  | StopQueryAt orderKey
  deriving (Show, Eq, Functor)

allEvents :: key -> QueryRange key orderKey
allEvents key = QueryRange key StartFromBeginning NoQueryLimit

eventsUntil :: key -> orderKey -> QueryRange key orderKey
eventsUntil key end = QueryRange key StartFromBeginning (StopQueryAt end)

eventsStartingAt :: key -> orderKey -> QueryRange key orderKey
eventsStartingAt key start = QueryRange key (StartQueryAt start) NoQueryLimit

eventsStartingAtUntil :: key -> orderKey -> orderKey -> QueryRange key orderKey
eventsStartingAtUntil key start end = QueryRange key (StartQueryAt start) (StopQueryAt end)

eventsStartingAtTakeLimit :: key -> orderKey -> Int -> QueryRange key orderKey
eventsStartingAtTakeLimit key start maxNum = QueryRange key (StartQueryAt start) (MaxNumberOfEvents maxNum)

-- | An event along with the keys from the particular event stream it was
-- queried from.
data StreamEvent key orderKey event
  = StreamEvent
  { streamEventKey :: !key
  , streamEventOrderKey :: !orderKey
  , streamEventEvent :: !event
  } deriving (Show, Eq, Functor, Foldable, Traversable)
