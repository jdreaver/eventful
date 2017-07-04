{-# LANGUAGE DeriveFunctor #-}
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
  ) where

-- | This type defines how to query an event stream. It defines the stream key
-- and the start/stop points for the query.
data QueryRange key position
  = QueryRange
  { queryRangeKey :: key
  , queryRangeStart :: QueryStart position
  , queryRangeLimit :: QueryLimit position
  } deriving (Show, Eq)

-- | This type defines where an event store query starts.
data QueryStart position
  = StartFromBeginning
  | StartQueryAt position
  deriving (Show, Eq, Functor)

-- | This type is used to limit the results of a query from an event store.
data QueryLimit position
  = NoQueryLimit
  | MaxNumberOfEvents Int
  | StopQueryAt position
  deriving (Show, Eq, Functor)

allEvents :: key -> QueryRange key position
allEvents key = QueryRange key StartFromBeginning NoQueryLimit

eventsUntil :: key -> position -> QueryRange key position
eventsUntil key end = QueryRange key StartFromBeginning (StopQueryAt end)

eventsStartingAt :: key -> position -> QueryRange key position
eventsStartingAt key start = QueryRange key (StartQueryAt start) NoQueryLimit

eventsStartingAtUntil :: key -> position -> position -> QueryRange key position
eventsStartingAtUntil key start end = QueryRange key (StartQueryAt start) (StopQueryAt end)

eventsStartingAtTakeLimit :: key -> position -> Int -> QueryRange key position
eventsStartingAtTakeLimit key start maxNum = QueryRange key (StartQueryAt start) (MaxNumberOfEvents maxNum)
