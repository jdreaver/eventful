<!--- -*- mode: Markdown;-*- -->

# Event Stores

Using events to change state is no good unless we can actually persist the
events somewhere. In `eventful`, we do that using
an
[EventStore](https://hackage.haskell.org/package/eventful-core/docs/Eventful-Store-Class.html#t:EventStore).
Before diving into the API, lets discuss some concepts related to event streams.

## Streams of events

Events don't exist in a vacuum; in most real-world scenarios the events we
receive have some natural association with other events. An example unrelated
to event sourcing is a stream of pricing data from a particular stock in the
stock market. For example, we could have a stream of bid quotes from Google's
stock (GOOG):

```json
{
  "price": 34.5,
  "time": "2017-05-17T12:00:00",
  "instrument": "GOOG"
}
{
  "price": 34.7,
  "time": "2017-05-17T13:00:00",
  "instrument": "GOOG"
}
{
  "price": 34.9,
  "time": "2017-05-17T14:00:00",
  "instrument": "GOOG"
}
```

There are a couple notable properties from this stream of events:
* The stream has an identity. In this case, it is "bid quotes for GOOG". If we
  were to store this stream in a database, a natural primary key would be the
  string `"GOOG"`.
* There is a natural ordering among the events; they can be ordered by
  `"time"`.

## Event sourced streams of events

In event sourcing, it's natural to think of the events for a particular piece
of state (a `Projection`) as a stream. Following the lead of the example above,
we can store give the stream an identity and also a natural ordering:
* It is common to use
  a [UUID](https://en.wikipedia.org/wiki/Universally_unique_identifier) to
  identify event sourced state streams.
* For each stream, we can order the events by a strictly increasing sequence of
  integers. In `eventful`, this is represented by
  the
  [EventVersion](https://hackage.haskell.org/package/eventful-core/docs/Eventful-Store-Class.html#t:EventVersion) type.

Here's an example of a possible event stream for our [Counter](./Counter.html):

```json
{
  "uuid": "123e4567-e89b-12d3-a456-426655440000",
  "type": "CounterIncremented",
  "amount": 3,
  "eventVersion": 0
}
{
  "uuid": "123e4567-e89b-12d3-a456-426655440000",
  "type": "CounterDecremented",
  "amount": 1,
  "eventVersion": 1
}
{
  "uuid": "123e4567-e89b-12d3-a456-426655440000",
  "type": "CounterReset",
  "eventVersion": 2
}
```

## Basic `EventStore` usage

The `EventStore` interface in `eventful` has two primary functions:
* `storeEvents`: Store a list of events to a given stream specified by the
  `UUID`
* `getEvents`: Retrieve events from the given `UUID` stream

Simple right? There are multiple event store backends included in `eventful`.
In the following example we are going to use the in-memory store from
`eventful-memory`.

The event store type `EventStore serialized m` has two type parameters:
* `serialized` is the serialization type. In our case, we don't really need to
  serialize so we can just use `CounterEvent`.
* `m` is the monad the event store operates in. For the in-memory store, that
  is the `STM` monad.

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

module EventStore where

import Control.Concurrent.STM
import Eventful
import Eventful.Store.Memory

import Counter

counterStoreExample :: IO ()
counterStoreExample = do
  -- First we need to create our in-memory event store.
  tvar <- eventMapTVar
  let
    writer = tvarEventStoreWriter tvar
    reader = tvarEventStoreReader tvar

  -- Lets store some events. Note that the 'atomically' functions is how we
  -- execute STM actions.
  let
    uuid = read "123e4567-e89b-12d3-a456-426655440000"
    events =
      [ CounterIncremented 3
      , CounterDecremented 1
      , CounterReset
      ]
  _ <- atomically $ storeEvents writer AnyVersion uuid events

  -- Now read the events back and print
  events' <- atomically $ getEvents reader (allEvents uuid)
  print events'
```

Output:

```
[ StreamEvent
  { streamEventProjectionId = 123e4567-e89b-12d3-a456-426655440000
  , streamEventVersion = EventVersion {unEventVersion = 0}
  , streamEventEvent = CounterIncremented 3
  }
, StreamEvent
  { streamEventProjectionId = 123e4567-e89b-12d3-a456-426655440000
  , streamEventVersion = EventVersion {unEventVersion = 1}
  , streamEventEvent = CounterDecremented 1
  }
, StreamEvent
  { streamEventProjectionId = 123e4567-e89b-12d3-a456-426655440000
  , streamEventVersion = EventVersion {unEventVersion = 2}
  , streamEventEvent = CounterReset
  }
]
```

This section of the tutorial obviously glossed over many details of the
`EventStore`. The main part of the documentation will cover those details.
