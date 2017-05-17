<!--- -*- mode: Markdown;-*- -->

# Manipulating state with events

The core concept behind event sourcing is your current state should be derived
from past events. To illustrate this, we will use an extremely simple example
of an integer "counter". The user can increment, decrement, or reset the
counter to zero. The purpose of this example is not to present a compelling
business use case for event sourcing. It is here to simply introduce event
sourcing.

## Counter State

The state for our counter is laughably simple:

```haskell
module Counter where

import Eventful

newtype Counter = Counter { unCounter :: Int }
  deriving (Show, Eq)
```

Our `Counter` is just a simple newtype wrapper around an `Int`.

In a non event sourced world, interacting with the `Counter` would probably
involve a few functions on the `Counter`:

```haskell
incrementCounter :: Counter -> Int -> Counter
incrementCounter (Counter count) amount = Counter (count + amount)

decrementCounter :: Counter -> Int -> Counter
decrementCounter (Counter count) amount = Counter (count - amount)

resetCounter :: Counter -> Counter
resetCounter _ = Counter 0
```

You could imagine these functions being wrapped in some sort of CLI or a REST
API. Note how the functions are written in an imperative tone, and they
directly modify the state. You could imagine the integer representing the count
being stored directly as in integer in some database.

## Moving to events

Now what if your boss comes up to you one day and says "hey, we think users
often make mistakes when incrementing the counter. We want to know how often an
increment is followed by a decrement of a smaller amount." With our current
model, this is simply not possible without some detective work (hello log
analysis! That is, if you have logs...).

If we had stored our state changes as events, we could easily give our boss
what he/she wants! What would some events look like in our case? How about this:

```haskell
data CounterEvent
  = CounterIncremented Int
  | CounterDecremented Int
  | CounterReset
  deriving (Show, Eq)
```

Note the parallels with our previous state modifying function. In this case, we
use the *past* tense to describe events. That is, an event is something that
has already occurred, and we are simply storing that fact.

## Using events to replay state

So we have some events, how do we use them? If events are records of what
happened in the past, then we want out internal state to be a function of these
facts. Let's write a function that can handle each event:

```haskell
handleCounterEvent :: Counter -> CounterEvent -> Counter
handleCounterEvent (Counter count) (CounterIncremented amount) = Counter (count + amount)
handleCounterEvent (Counter count) (CounterDecremented amount) = Counter (count - amount)
handleCounterEvent _ (CounterReset) = Counter 0
```

Easy right?

Now, let's introduce the concept of a `Projection` in eventful. First we'll
create one for a `Counter` and then we can discuss details:

```haskell
counterProjection :: Projection Counter CounterEvent
counterProjection =
  Projection
  { projectionSeed = Counter 0
  , projectionEventHandler = handleCounterEvent
  }
```

A `Projection` is a pair of a "seed" and an event handler. A seed is simply the
default value for the projection; we always have to know what to start with
when we don't have events. The event handler tells the projection how to apply
events to state. Note that the projection has two type parameters for the state
and event types.

## Convenience functions for `Projection`

`eventful` comes with some convenience functions to rebuilt the current state
for a `Projection` from a list of events, and to show all `Projection` states.

```haskell
myEvents :: [CounterEvent]
myEvents =
  [ CounterIncremented 3
  , CounterDecremented 1
  , CounterReset
  ]

myLatestCounter :: Counter
myLatestCounter = latestProjection counterProjection myEvents
-- Counter {unCounter = 0}

allMyCounters :: [Counter]
allMyCounters = allProjections counterProjection myEvents
-- [ Counter {unCounter = 0}
-- , Counter {unCounter = 3}
-- , Counter {unCounter = 2}
-- , Counter {unCounter = 0}
-- ]
```
