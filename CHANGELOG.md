# eventful Changelog

## 0.1.2

* Refactored `ProcessManager` so it acts more like a read model that emits
  commands and events. Got rid of `ProcessManagerRouter`
* Fix race condition in postgres event store where sequence numbers won't be
  monotonically increasing when multiple writers are writing to the events
  table. Fixed with an explicit lock on the events table.
* Restructured record types for events paired with version numbers and global
  sequence numbers.

## 0.1.1

* Small pedantic fixes for examples and test suites.
* Added links to the github repo in the cabal files.

## 0.1.0

Initial Hackage release of `eventful`. It is already fairly full-featured along
with some documentation and examples.
