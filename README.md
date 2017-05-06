# Eventful: Haskell Event Sourcing + CQRS Library

`eventful` is a Haskell library intended to be used as a basis for applications
built using event sourcing. In particular, `eventful` has a number of
independent features that users can choose from to help construct their
application:

* Robust event stream storage using our `EventStore` type. There are multiple
  backends implemented, including in-memory, SQLite, PostgreSQL, and AWS
  DynamoDB.
* Convenience layer with common ES/CQRS abstractions, including `Projection`,
  `Aggregate`, and `ProcessManager`. All of these integrate with `EventStore`
  so you get transparent integration with your underlying event storage.

## Why should you use eventful?

One of the attractive features of event sourcing is how simple the fundamental
abstractions are. At its base, all you really need are some events and a fold
function to reconstruct state using those events. Storing those events is
fairly straightforward with a SQL table that only has a few columns. In fact,
one common piece of advice given to newcomers is to **not** use a framework for
your ES/CQRS application. That way you can build the fundamental knowledge of
how the architecture works by yourself.

However, there are some compelling features of `eventful` that make it
attractive to use for your application:

* The API is simple, and the implementation strives to use the most vanilla
  Haskell possible. This makes the API simpler to understand, but it also means
  the individual pieces of `eventful` are independent and orthogonal because
  there are no crazy type-level abstractions binding them together. If you want
  to use `EventStore` without `Projection`, `Aggregate`, an event bus, etc,
  that is totally doable.
* All of the event stores have the same API, the only difference being the
  Monad they operate in. For example, the SQL-based stores are built with
  `persistent`, so they use `SqlPersistT`. The default in-memory store uses
  `STM`. This means you can swap out event stores as your application evolves,
  and also use in-memory stores to speed up testing.
* If the Monad for your event store has transactional semantics, like
  `SqlPersistT` and `STM`, then you can perform multiple actions in one
  transaction or even use `eventful` alongside non-event sourced data in your
  application, and still get transactional guarantees.
* No serialization method is forced upon the user for any of the event stores.
  A lot of functions in `eventful` carry around a type parameter called
  `serialized`, which is the type you want to serialize/deserialize events to.
* Users can handle deserialization however they want. You can ignore errors,
  throw an exception on errors, etc. You can have on event ADT per projection,
  or throw all of the events in your application in a single ADT for
  ease-of-use. The choice is up to you.

In summary, `eventful` is a toolbox that you choose pieces from, not an
overarching framework that imposes design decisions on you.

## Examples

See the [examples](./examples) directory for some example programs.
