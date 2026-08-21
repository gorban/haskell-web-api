# Revision history for haskell-web-api

## 0.1.2.0

* Account forms and action dispatch now share `accountActions :: ActionCodec AccountActionTarget AppRequestContext AccountAction`, eliminating duplicated action paths, methods, and field lookup helpers while preserving localized `422` patches for expected domain rejection.
* `web-api` now builds its `HarchWeb.Application` through the new `HarchWeb.Site` wrapper, keeping app-owned routes and shell details in the composition root while avoiding direct construction of the lower-level framework record.
* `web-api` now pools its PostgreSQL connections through a new `WebApi.Postgres.Pool` instead of opening a fresh libpq connection per query, sized by a new `DATABASE_POOL_CAPACITY` environment variable.
* **Breaking:** enabled `-Wpartial-fields` and fixed the flagged type. `ProfilePageModel`'s four constructors each now carry a single nested detail record (`SignedOutProfilePageDetails`, `PendingProfilePageDetails`, `AuthenticatedProfilePageDetails`, `UnavailableProfilePageDetails`, each with a type-specific field prefix matching this module's own `HomePageModel`/`SecondPageModel`/`NotFoundPageModel` convention) instead of record fields directly on the sum type. No other behavior change.

## 0.1.1.0

* `web-api` now serves as the composition root over the new `HarchWeb` facade instead of owning only a stdout placeholder.
* App-facing seams are being kept pure where possible so route matching, page rendering, layout decisions, and future config parsing can be Unit-tested before IO adapters are added.
* The intended PR-ready scope for `web-api` is to remain responsible for app routes, pages, config, and startup while delegating shared SSR/server/runtime concerns to `HarchWeb`.
* The executable and tests now target the new facade startup path rather than the previous direct banner-writing stub.
* `web-api` now includes a tiny bundled `navigation.js` asset, and the shared shell can emit and serve that asset through configured static roots.

## 0.1.0.1

* Upgraded GHC to 9.14.1 and Base to 4.22.0.0

## 0.1.0.0

* web-api is not a real API yet.
* Hooks in test-core package for testing purposes.
