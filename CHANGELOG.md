# eventful Changelog

## 0.1.3

* Added `EventStoreQueryRange` to the APIs of the event store and the globally
  ordered event store. This allows the user to specify optional start and stop
  points for the query.
* Added versions of the `MonadState` event stores that can include other state
  along with the `EventMap`. These are called `embeddedStateEventStore` and
  `embeddedStateGloballyOrderedEventStore`.
* Made `Projection` and instance of `Contravariant` so it is easier to change
  the `event` type parameter.
* Added a `GloballyOrderedProjection` abstraction to make projecting from
  globally-ordered event streams simpler.
* Added useful `projectionMapMaybe` function.
* Added the `runEventStoreUsing` and `runGloballyOrderedEventStoreUsing`
  functions for running event stores in different monads than the original.

## 0.1.2

* Refactored `ProcessManager` so it acts more like a read model that emits
  commands and events. Got rid of `ProcessManagerRouter`
* Fix race condition in postgres event store where sequence numbers won't be
  monotonically increasing when multiple writers are writing to the events
  table. Fixed with an explicit lock on the events table.
* Restructured record types for events paired with version numbers and global
  sequence numbers.
* Created an in-memory event store for `MonadState` in `eventful-memory`.

## 0.1.1

* Small pedantic fixes for examples and test suites.
* Added links to the github repo in the cabal files.

## 0.1.0

Initial Hackage release of `eventful`. It is already fairly full-featured along
with some documentation and examples.
