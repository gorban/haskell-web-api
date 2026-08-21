# Changelog

## 0.1.2.0

1. Added declarative `ActionCodec` endpoint definitions shared by typed client-action forms and server
   decoding. The protocol now distinguishes unknown paths (`404`), method mismatches (`405` with `Allow`),
   malformed matched fields (`400`), and application validation responses (`422`).
2. Added a new `HarchWeb.Site` wrapper that lets composition roots assemble typed route definitions, shared page-shell configuration, and route-derived navigation without directly building the lower-level `Application` record.
3. Switched the first-party `web-api` composition root onto that `HarchWeb.Site` path while preserving current page/API behavior and keeping the route-aware shell output stable under the existing test suite.
4. Replaced destructive client-action queue draining with retained, control-local capture ownership and
   visible pending/recoverable/cancelled states; native action submission is now an explicit capability.
5. Added `HarchWeb.Api`: low-level `ApiEndpoint` compatibility matching plus a shared-route
   `apiRouteEndpoint` declaration with typed query/header/cookie/form request values, one declared body consumer, typed response
   encoders, RFC 9110 `Accept` selection, `Vary: Accept`, and `406` for incompatible representations;
   request/response codecs, `Content-Type`-selected request-body decoding, RFC 9110 `Accept` negotiation,
   and `apiEndpointMiddleware`, an opt-in `Wai.Middleware` an application wraps around its own
   `Wai.Application` to dispatch declared paths. Added `HarchWeb.Api.Multipart`, a bounded, incremental
   RFC 7578 `multipart/form-data` consumer with size limits and temporary-file spooling for uploads. Both
   are standalone, fully-tested library capabilities layered onto the existing `RouteCodec`/`ApiRoute`
   dispatcher rather than replacing it. The `examples/custom-api` guide now includes a compiled,
   tested demonstration of the full path: negotiated JSON/custom-media-type responses, a decoded JSON
   request body, and a multipart upload.
6. `HarchWeb.Api`'s `OPTIONS` now answers `204 No Content` with an `Allow` header instead of falling
   through to `405`, and `ApiResponseBody` gained `apiResponseStatus` so a target can render an ordinary
   non-`200` outcome (e.g. `422`). Added `runServerWithWaiMiddleware`/`withLocalTestServerForApplication`
   so an application can compose `apiEndpointMiddleware` in front of a real running (or locally
   test-served) server, not just a bare `Wai.Application` in a unit test. `examples/two-pages` now
   includes a compiled, real-browser-tested `/native-upload` page: a CSRF-protected native multipart
   file-upload form dispatched through `apiEndpointMiddleware`, using a single-use server-held CSRF
   token and `consumeMultipartRequestBodyWith`'s early per-part rejection so an invalid or absent CSRF
   field is rejected before a later file part is ever spooled to disk, and carrying no
   `data-harch-action` attribute so the inline capture kernel never intercepts it. `TestCore.Browser`
   gained `setInputFiles` to drive that form's file input from an E2E scenario.
7. Added `ApiMultipartRequestBody` to the typed API endpoint declaration. It supplies a handler with a
   single-use scoped multipart consumer backed by an explicitly selected storage adapter and
   `MultipartLimits`; duplicate consumption and parser failures remain typed outcomes, and uploads retain
   their existing promote-or-discard lifecycle.
8. `web-api` now throttles login attempts: a new `LoginAttemptStore` capability (with a PostgreSQL
   adapter and `login_attempts` migration) backs a per-identifier and per-account lockout using
   `HarchWeb.LoginProtection`'s existing policy, an unknown-identifier login now runs a dummy Argon2id
   verification so its timing matches a known identifier's wrong-password rejection, and a recovery-code
   attempt is throttled the same way before its up-to-eight-hash KDF comparison runs.
9. Typed API endpoint transport failures (400/406/413/415) now render through the endpoint's own
   declared response representation when its response type can carry the failure sentence, instead
   of always answering `text/plain`; an endpoint whose response type cannot carry it gets an empty
   body with no `Content-Type` claim rather than a mismatched one.
10. TOTP login verification now compares codes with a constant-work comparison instead of derived
    `Eq`, and rejects a code whose matched time-step counter is at or below the account's last
    accepted counter (persisted in a new `account_totp.last_used_totp_counter` column, updated by an
    atomic conditional store operation), closing the ~90-second window in which an observed code
    could previously be replayed against the login endpoint.
11. The HTTP-to-HTTPS upgrade redirect no longer echoes the request's own `Host` header into its
    `Location`, closing an open redirect a client (or an intermediary cache) could previously trigger
    by setting that header on a plaintext request. `RequestPolicyConfig` now carries an explicit
    canonical `httpsRedirectAuthority`, derived from this app's own HTTPS listener when it has one and
    otherwise from the already-required `PUBLIC_BASE_URL`; the redirect simply does not fire when
    neither source resolves an authority.
12. **Breaking:** trusting `X-Forwarded-For`/`-Proto`/`-Prefix`/`Forwarded` is now a property of the
    connecting peer, not a global on/off flag: any client could previously spoof these headers to
    poison `client.address` observability and downgrade its own connection's HSTS/redirect behavior
    just by sending them. `RequestPolicyConfig`'s `trustForwardedHeaders :: Bool` is replaced by
    `forwardedHeaderTrust :: ForwardedHeaderTrust`, checked against the request's actual TCP peer
    address, not the header content; a Unix-domain-socket peer is always trusted once forwarding is
    enabled at all, since it cannot be spoofed remotely. The `TRUST_FORWARDED_HEADERS` environment
    variable is replaced by `TRUSTED_FORWARDED_PROXIES` (a comma-separated CIDR block list, e.g.
    `10.0.0.0/8,172.16.0.0/12`), defaulting to this app's own unique HTTPS listener host when present.
13. **Breaking:** removed `HarchWeb.Acme`'s never-wired native ACME protocol client (account/order/
    authorization/finalization requests, RSA-4096 key and CSR generation, RS256 JWS signing) and its
    `openssl`-subprocess adapter, which resolved the `openssl` binary through `$PATH` and exported
    that raw command runner publicly. This code had no production callers: ACME certificate
    acquisition is, and has always been, exclusively certbot-backed (`AcmeConfig` requires a
    `CertbotConfig`); nothing else changes for existing certbot-based deployments. `HarchWeb.Acme`'s
    module documentation now states this plainly.
14. **Breaking:** removed `HarchWeb.Acme`'s hand-rolled `ReadP` JSON parser, `JsonValue` tree, and
    field accessors, along with their re-export through the `HarchWeb.Acme` facade. This parsing
    code was, like the client removed above, unreachable in production: OTLP trace/metric export
    (`HarchWeb.Observability.Otlp.Wire`) only ever used the three byte-builder JSON encoders it sat
    alongside. Those three encoders (`jsonArrayBytes`, `jsonObjectBytes`, `jsonStringBytes`) are
    unchanged in behavior and now live at the small, directly-importable `HarchWeb.Acme.Json` module
    instead of the `HarchWeb.Acme` facade.
15. **Breaking:** `toWaiApplication` now applies the opt-in concurrent-in-flight-request admission
    gate unconditionally, sourced from the application's own `requestConcurrencyLimit` (a `Nothing`
    limit preserves the existing unbounded default). Previously only `HarchWeb.Server.Runtime` and
    `HarchWeb.Server.LocalTest` composed that gate themselves; an application building its own
    `Wai.Application` directly from `toWaiApplication` — the only path the public facade exposes for
    that — silently never had a configured limit enforced at all. `toWaiApplication`'s type changes
    from `Application route action context -> Wai.Application` to `... -> IO Wai.Application`, since
    the gate's admission counter must be allocated once per running server and shared across every
    request, not rebuilt per request; call it once at server startup rather than per request.
16. **Breaking:** `DatabaseConfig` gained `databaseConnectTimeoutSeconds`, applied as
    `connect_timeout` on every libpq connection the runtime query path opens. Previously a wedged
    or unreachable database server left a connection attempt waiting indefinitely, pinning a
    request thread and, with a concurrency limit configured, eventually starving every request
    behind it. Configured via the new `DATABASE_CONNECT_TIMEOUT_SECONDS` environment variable
    (default `10`); `sslmode` is unchanged and remains `prefer` (libpq's own default), since
    requiring TLS unconditionally would refuse every connection to a Postgres server that doesn't
    have it configured, including this project's own default local/CI setup — see the AY note in
    `docs/design-guidance.md` for why that half of the finding is deferred rather than defaulted on.
17. **Breaking:** a taken username during registration now gets its own response (422, naming the
    `registration-username` field) instead of silently reporting the same "check your inbox"
    outcome a genuine registration or a taken *email address* gets. `AccountStore.createPendingAccount`
    returns a new `CreatePendingAccountOutcome` (created / email taken / username taken) instead of
    `Bool`, and `RegistrationResult` gained `RegistrationUsernameTaken`; the taken-email and
    newly-created outcomes remain byte-identical to each other, preserving the existing
    anti-enumeration protection for email addresses specifically. The pending-account insert now
    targets `ON CONFLICT (email_normalized) DO NOTHING` (was untargeted), paired with an upfront
    username-availability check run only when a username was supplied.
18. **Breaking:** `dataAttribute`/`dataFlag` now take a validated `DataAttributeSuffix` instead of
    raw `Text` (`mkDataAttributeSuffix` restricts it to non-empty `[a-z0-9-]+`), and `href`/
    `pageLink` now take a validated `SafeUrl` instead of raw `Text` (`mkSafeUrl` allowlists relative
    references and `http`/`https` schemes, rejecting `javascript:`/`data:`/`vbscript:`/etc., and
    strips embedded whitespace/control characters before reading the scheme so an obfuscated
    literal cannot bypass it). Attribute values are HTML-escaped before rendering, but an attribute
    *name* is written into markup with no escaping at all, so an unvalidated `data-*` suffix could
    previously inject an arbitrary event-handler attribute; `href` previously accepted any scheme,
    including ones a browser will execute as script when a link is followed. Both new types carry
    an `IsString` instance, so every existing `OverloadedStrings` literal call site (including
    inside `[harch| ... |]` quasiquoted markup) keeps compiling unchanged; a caller building either
    value from a runtime `Text` value must go through the explicit `mkDataAttributeSuffix`/
    `mkSafeUrl` smart constructor and handle rejection.
19. The `[harch| ... |]` quasiquoter now resolves every framework identifier it splices (`text`,
    `element`, `dataAttribute`, native tag constructors, attribute constructors, …) through a
    compile-time-quoted name against the framework's own implementation module, instead of an
    unqualified name resolved dynamically at the splice site. Previously, a component with a
    parameter or local binding named `value`, `name`, `method`, or `text` (among others) could
    silently rebind the framework's own constructor at every markup literal that name was visible
    to — usually an inscrutable type error, and in the worst case an escaping bypass with no
    diagnostic. An unsupported native element name (e.g. a typo'd tag) is now a parse-time failure
    with its own source position, rather than a deferred lowering-time failure.
20. A `{...}` markup interpolation containing a bare Template Haskell name quote (e.g. `{'Just}`)
    now fails to compile with a clean, positioned error instead of crashing the calling module's
    compile with an uncaught exception from deep inside a parsing dependency.
21. **Breaking:** `HarchWeb.GoogleWorkspace.mkGoogleWorkspaceAccessTokenProvider` now caches the
    minted Gmail API access token behind a new `GoogleWorkspaceTokenCache` (first argument,
    allocated once via `newGoogleWorkspaceTokenCache`), re-minting only once the token is within 60
    seconds of Google's returned `expires_in`, instead of performing a full RSA-signed token
    exchange on every call. `HarchWeb.Gmail.runGmailHttpRequest` now takes an explicit
    `HttpClient.Manager` (first argument) instead of creating a new TLS manager per request,
    enabling connection reuse.

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
