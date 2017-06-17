{-# LANGUAGE DeriveFunctor #-}

module Eventful.Store.Queries
  ( EventStoreQueryRange (..)
  , EventStoreQueryStart (..)
  , EventStoreQueryLimit (..)
  , allEvents
  , eventsUntil
  , eventsStartingAt
  , eventsStartingAtUntil
  , eventsStartingAtTakeLimit
  ) where

-- | This type defines how to query an event stream. It defines both where to
-- start and where to stop in the stream.
data EventStoreQueryRange a
  = EventStoreQueryRange
  { eventStoreQueryRangeStart :: EventStoreQueryStart a
  , eventStoreQueryRangeLimit :: EventStoreQueryLimit a
  } deriving (Functor)

-- | This type defines where an event store query starts.
data EventStoreQueryStart a
  = StartFromBeginning
  | StartQueryAt a
  deriving (Show, Eq, Functor)

-- | This type is used to limit the results of a query from an event store.
data EventStoreQueryLimit a
  = NoQueryLimit
  | MaxNumberOfEvents Int
  | StopQueryAt a
  deriving (Show, Eq, Functor)

allEvents :: EventStoreQueryRange a
allEvents = EventStoreQueryRange StartFromBeginning NoQueryLimit

eventsUntil :: a -> EventStoreQueryRange a
eventsUntil end = EventStoreQueryRange StartFromBeginning (StopQueryAt end)

eventsStartingAt :: a -> EventStoreQueryRange a
eventsStartingAt start = EventStoreQueryRange (StartQueryAt start) NoQueryLimit

eventsStartingAtUntil :: a -> a -> EventStoreQueryRange a
eventsStartingAtUntil start end = EventStoreQueryRange (StartQueryAt start) (StopQueryAt end)

eventsStartingAtTakeLimit :: a -> Int -> EventStoreQueryRange a
eventsStartingAtTakeLimit start maxNum = EventStoreQueryRange (StartQueryAt start) (MaxNumberOfEvents maxNum)
