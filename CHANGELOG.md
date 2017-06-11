# eventful Changelog

## 0.1.2

* Refactored `ProcessManager` so it acts more like a read model that emits
  commands and events. Got rid of `ProcessManagerRouter`
* Restructured record types for events paired with version numbers and global
  sequence numbers.

## 0.1.1

* Small pedantic fixes for examples and test suites.
* Added links to the github repo in the cabal files.

## 0.1.0

Initial Hackage release of `eventful`. It is already fairly full-featured along
with some documentation and examples.
