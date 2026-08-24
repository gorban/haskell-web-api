# Changelog

## 0.1.2.0

* **Breaking: broad public API overhaul.** Affected: framework composition and routes; API/action declarations; server, transport, and security configuration; markup; ACME, Google, and OTLP services; and app persistence/configuration.
* (added) Typed SSR, action, and API route composition; bounded form/multipart body handling; and browser interaction capture with native fallback.
* (added) Security hardening: typed untrusted inputs, request/Argon2/multipart limits, secure redirects/proxy trust, constant-time secrets and TOTP replay resistance, and CSRF/session binding.
* (added) Operational boundaries: connection pooling/timeouts, transactional versioned migrations, async-safe resource ownership, request admission, explicit ACME/HTTP-manager props, and observability boundaries.
* (added) Account lifecycle: registration/verification, MFA enrollment/recovery, sessions/profile actions, and adaptive login throttling.
* Tooling, package setup, test organization, CI reproducibility, and coverage diagnostics were substantially revised.

## 0.1.1.0

1. Began extracting the future SSR-first web framework into the separate `harch-web` package so shared web architecture can evolve independently from the example app.
2. Defined the first `HarchWeb` facade boundary around application description, route matching, page rendering, not-found handling, shared page shell attachment, and the final server startup seam.
3. Repositioned `web-api` as the application composition root over that facade, keeping app-specific routes, pages, config, and startup wiring outside the framework package.
4. Added initial unit/integration coverage for the stub facade path in `web-api` and introduced a first dedicated `harch-web` test suite for route matching and server-boundary behavior.
5. Documented the new package layout in the root README and expanded local debugger/setup guidance for running package test suites and spec preprocessors from editor tooling.
6. Cleaned related repository metadata while landing the new framework boundary, including `core` package terminology updates, `hspec-expectations-match` extra-source-file cleanup, and repository-wide changelog heading normalization without inline dates.
7. Established the intended PR scope for `HarchWeb` as tracked in `TASKS.md`: pure SSR route/page/layout/config seams first, then a thin server adapter, then effect-backed data access and a tiny progressive-enhancement navigation runtime.
8. Made typed API response negotiation parameter-aware: an `Accept` media parameter now selects only a
   declared compatible `Content-Type`, while parameters after `q` remain RFC 9110 accept extensions.
9. Added `streamingResponseEncoder` for typed API endpoints. Its WAI stream remains request-scoped and
   passes through the shared protocol response interpreter without materializing a lazy response body.
10. Fixed `apiRouteEndpointFamilyDefinition` raising an uncaught error instead of rendering `404` for a
    path no declared endpoint owns, when its route family is used standalone (not combined with a
    catch-all family). Migrated `examples/custom-api`'s `App.Api.Declarative` onto the typed
    `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` pair, replacing the legacy
    `apiEndpoint`/`apiEndpointMiddleware` compatibility helpers.
11. Added `apiEndpointResponseObservabilityAttributes`/`apiEndpointResponseLogEntries` to `ApiResponse`,
    so a typed endpoint handler can attach private diagnostics to its rendered protocol response, the
    same capability `ResponseBody` already gives page routes. Both default to empty and have no
    encoder, so they cannot leak into a response body.
12. Migrated `examples/multipart-upload`'s `App.MultipartUpload`/`App.App` onto the typed
    `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` pair, replacing `apiEndpointMiddleware`.
    No application in the repository still uses the legacy `ApiEndpoint`/`apiEndpointMiddleware` or
    `apiRouteEndpointMiddleware` compatibility helpers.
13. **Breaking:** deleted those now-unused legacy `ApiEndpoint`/`apiEndpointMiddleware`/
    `apiRouteEndpointMiddleware` compatibility helpers from `harch-web` outright, along with
    `apiHttpResponseToWaiResponse`/`apiAllowHeaderValue` (unused once both middlewares were gone).
    `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` (composed into a `HarchWeb.Site.Site`)
    is now the only supported way to dispatch a typed API endpoint table.
14. Added an opt-in concurrent-in-flight-request admission gate. A request beyond the configured limit
    receives an immediate `503` before route parsing, middleware, observability, or body reads, instead
    of queueing; the runtime's established unbounded default (a worker per accepted connection, with no
    admission control) is unchanged unless a limit is configured. `web-api` wires it from
    `REQUEST_MAX_CONCURRENT`.

## 0.1.0.1

1. Dropped Windows native support. Windows native was last fully supported in
   [0.1.0.0](https://github.com/gorban/haskell-web-api/tree/v0.1.0.0), but was later removed because, even
   after an immense effort to produce JavaScript output from Haskell (e.g. from GHCJS or the newer GHC with
   JavaScript backend), it just is not community-supported (see WIP PRs for unresolved issue
   [ghcjs#834](https://github.com/ghcjs/ghcjs/issues/830), for
   [ghcjs#1](https://github.com/ghcjs/ghcjs/issues/833) and its specific
   [ghc#1](https://github.com/gorban/ghc/pull/1) version it needs). If you want to run this on Windows, you
   can use WSL2 or Docker (configured for Linux containers).
2. Simplified generate-code-coverage-report to just be an .sh script (no more PowerShell polyglot).
3. Split off this setup guide into its own file, and added a link to it from the main README.
4. Upgraded GHC to 9.14.1 and Base to 4.22.0.0.

## 0.1.0.0

1. Initial release.
2. Completely lacks a real web API, but has a putStrLn in the main application executable as a placeholder.
   The main focus of this release is to set up the project structure, build system, testing framework, and
   CI/CD pipeline with code coverage reporting. The actual web API implementation will come in future
   releases.
