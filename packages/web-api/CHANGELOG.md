# Revision history for haskell-web-api

## 0.1.2.0

* **Breaking: substantial application API and persistence reorganization.** Affected: configuration, account/page/action models, login/MFA/session stores, PostgreSQL adapters, and migrations.
* (added) Typed account actions, SSR site composition, localized account/profile pages, registration/verification, MFA enrollment/recovery, and session workflows.
* (added) The typed `WebApi.Api.Endpoints` composition module and configuration for request target, header, path/query, network-progress, slowloris, and concurrent-request limits (`REQUEST_*`).
* (added) PostgreSQL pooling, connection timeouts, transactional versioned migrations, epoch-based durable security time, and atomic login-attempt admission/settlement.
* (added) Login hardening: unknown-user timing protection, shared password/MFA throttles, TOTP replay prevention, fail-closed persistence outcomes, and session-bound CSRF.

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
