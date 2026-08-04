# Revision history for harch-web

## 0.1.2.0

* Added `HarchWeb.Action`: a declarative, applicative `ActionCodec` whose endpoint declarations print
  typed form targets and decode matched requests. It provides deterministic accumulated field errors and
  explicit unknown-path, method-negotiation, and malformed-input outcomes.
* Added typed `HarchWeb.Controls.actionForm` attributes that prevent framework-owned action, method, and
  capture markers from being overridden, and made `405` action responses derive an `Allow` header from
  the codec.
* Replaced destructive captured-action draining with a retained lifecycle coordinator: deferred handlers
  claim and settle work by identity, while pending, delayed, recoverable, and cancelled states stay local
  to the originating control. The inline kernel has a 12 KiB rendered-source regression budget.
* Added explicit control capabilities for exclusive client handling, native fallback, conditional leave
  warning, and retry policy declaration, plus typed recovery copy and accessible status/cancellation markup.
* Added a new `HarchWeb.Site` module with `Site`, `SiteRoute`, `simpleSite`, `pageSiteRoute`, and `buildSiteApplication` so small SSR apps can be described without directly constructing the full `Application` record.
* Kept the wrapper compatible with existing route codecs, page shells, body responses, and not-found status behavior, and covered the new path with dedicated unit tests.

## 0.1.1.0

* Initial stub facade package for the SSR framework boundary.
* Added the first public facade types and functions: `Application`, `Page`, `RouteCodec`, `application`, `matchRoute`, and `runServer`.
* Extended the facade to carry routed request context and to distinguish page responses from future API/data responses.
* Added package-local unit tests for route matching and the stub server boundary.
* Wired the package into first-party repository conventions with lowercase package naming and `Custom` setup that copies the root `LICENSE` file.
