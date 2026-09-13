# Revision history for core

## v0.1.2.0

* **Breaking: core setup/configuration helpers were substantially reorganized.** Affected: package setup, environment/config parsing, and prerequisite reporting.
* (added) Page-route generation and focused database/container/tracing prerequisite modules.

## 0.1.1.0

* Clarified package metadata to reference `test-core` instead of the retired `test-lib` name.

## 0.1.0.1

* Upgraded GHC to 9.14.1 and Base to 4.22.0.0

## 0.1.0.0

* Initial release with `Core.Control.Error` module providing `handleError` for ergonomic `ExceptT` error handling.
