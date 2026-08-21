# Revision history for harch-web

## 0.1.2.0

* **Breaking:** enabled `-Wpartial-fields` and fixed the two flagged types.
  `TlsCertificateSource`'s `ManualCertificateFiles`/`SharedCertificateFiles` constructors each now
  carry a single nested detail record (`ManualTlsCertificateFiles`, `SharedTlsCertificateFiles`)
  instead of record fields directly on the sum type, and `TlsStartupMode`'s `AwaitCertificateFiles`
  is now a plain positional `Maybe Int` rather than a single-field record. No other behavior change.
* Added an opt-in, non-blocking concurrent-in-flight-request admission gate:
  `RequestConcurrencyLimit`/`mkRequestConcurrencyLimit` (`HarchWeb.Security`) and
  `HarchWeb.Server.RequestExecution.concurrencyLimitedMiddleware`, composed around the same
  `Wai.Middleware` seam `runServerWithWaiMiddleware` already exposes and shared by both
  `HarchWeb.Server.Runtime`'s real listeners and `HarchWeb.Server.LocalTest.withLocalTestServer`, so a
  real-socket test observes the same admission behaviour a deployed runtime would. A request beyond the
  configured `RequestPolicyConfig`'s `requestConcurrencyLimit` gets an immediate `503` before route
  parsing, middleware, observability, or body reads; a held slot is always released — on ordinary
  completion or any exception — for the request's whole lifetime, including a streamed response.
  `Nothing`/absence preserves the framework's established unbounded behaviour: Warp 3.4.12 has no
  concurrent-request or connection-count setting of its own, so the runtime forks a worker per accepted
  connection with no admission control unless a limit is configured.
* **Breaking:** removed the legacy `ApiEndpoint`/`apiEndpoint`/`apiEndpointTarget`/`ApiMatchResult`/
  `matchApiEndpoints`/`respondApiMatch`/`apiEndpointMiddleware` compatibility table and the intermediate
  `apiRouteEndpointMiddleware` typed-WAI-middleware composition, along with `apiHttpResponseToWaiResponse`
  and `apiAllowHeaderValue` (both now unused once those middlewares were gone). Every application in
  this repository was already migrated onto `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition`
  before this removal; applications that still relied on the deleted helpers should migrate onto that
  pair, composed into a `HarchWeb.Site.Site` via `simpleSite`/`buildSiteApplication`/`toWaiApplication`.
* Added `apiEndpointResponseObservabilityAttributes`/`apiEndpointResponseLogEntries` to `ApiResponse`.
  A typed endpoint handler can now attach private diagnostics (a failure code, a downstream error
  detail) to its rendered `ProtocolResponse`, matching the capability `HarchWeb.Server.Response.ResponseBody`
  already gives page routes. Both default to `[]` via `apiResponse` and are overridable by record
  update like `apiEndpointResponseHeaders`; neither has a response encoder, so they cannot leak into
  a response body.
* Fixed `apiRouteEndpointFamilyDefinition`'s `routeResponse` raising an uncaught `error` instead of
  rendering an ordinary `404` for a path no declared endpoint owns, when its `apiRouteEndpointFamilyCodec`
  route family is used standalone rather than combined with a catch-all family via
  `HarchWeb.Routing.combineRouteCodecs`. `apiRouteEndpointFamilyCodec`'s own not-found route now renders
  correctly instead of crashing every unmatched request.
* Added bounded `application/x-www-form-urlencoded` request support: `urlEncodedFormBodyDecoder` preserves
  field order and duplicates, enforces an application-declared field-count cap, and rejects malformed
  percent escapes and invalid UTF-8. `ApiUrlEncodedFormRequestBody` declares it as an endpoint's sole body
  consumer; `formField` then reuses the accumulating typed field codec alongside query, header, and cookie
  declarations after decoding succeeds.
* Added `cookieField` to `HarchWeb.Api`'s accumulating `RequestCodec`. It extracts case-sensitive cookie
  names from every `Cookie` header, ignores malformed fragments, and reports repeated names as a typed
  duplicate-field error rather than selecting an arbitrary value.
* Extended the shared-route `apiRouteEndpoint` boundary with typed response values and declared pure
  representation encoders. It now negotiates a request's RFC 9110 `Accept` header, emits the selected
  `Content-Type`, preserves endpoint status and headers, merges `Vary: Accept` for alternate encodings,
  and returns `406 Not Acceptable` for explicit incompatibility. Invalid quality weights are rejected unless
  they are within the RFC's 0–1, three-decimal bound.
* Hardened `HarchWeb.Api.Multipart` ownership: removed the raw collector APIs that returned adapter
  values (including file paths). `withMultipartBodyWith` and `withMultipartRequestBodyWith` now provide
  the public consumption surface; file uploads are opaque, scope-bound values that applications either
  promote deliberately or allow the framework to discard.
* Added `HarchWeb.Api`: a declarative, method-aware `ApiEndpoint` declaration with path/method matching
  (`404`/`405`+`Allow`/`HEAD`/`OPTIONS` handling — `OPTIONS` is synthesized as `204 No Content` with an
  `Allow` header for any path with a declared endpoint, without running a handler or implementing CORS;
  an application composes its own CORS middleware in front if it needs preflight support), an
  accumulating-error `RequestCodec` for query, header, and cookie fields (with
  `apiRequestDataFromWaiRequest` to extract them from a real WAI request),
  `Content-Type`-selected buffered request-body decoding (`selectApiBodyDecoder`, with
  `jsonBodyDecoder`/`textBodyDecoder`/`bytesBodyDecoder` built-ins and `415`/`400` outcomes; the
  endpoint body reader supplies the distinct `413` outcome),
  `ResponseCodec` helpers for JSON/text/bytes bodies, RFC 9110 `Accept` header negotiation (quality
  weights, wildcards, specificity, `q=0`, and `406`), `respondApiMatch` to render an `ApiMatchResult`
  into a transport-agnostic `ApiHttpResponse` (status, headers, and an optional body, omitted for a
  `HEAD` match), and `apiEndpointMiddleware`, an opt-in `Wai.Middleware` (via `apiHttpResponseToWaiResponse`)
  that dispatches a request whose path matches a declared endpoint and falls through to the wrapped
  application for everything else. This is a fully-tested library capability; it is not yet the
  application's *default* request dispatcher, which remains the existing `RouteCodec`/`ApiRoute` pattern.
* Added `HarchWeb.Api.Multipart`: a bounded, incremental RFC 7578 `multipart/form-data` consumer. A pure
  boundary scanner never buffers more of a part's body than the boundary delimiter's length, so large
  file parts stream without full buffering; `Content-Disposition` field-name/filename extraction handles
  quoted values, backslash-escaped quotes, and semicolons inside quoted filenames (RFC 5987/6266 extended
  `filename*=` parameters are not supported). `consumeMultipartBody` drives the scanner against any
  chunked `IO ByteString` source, enforcing per-field, per-file, and part-count limits, keeping field
  values in memory and spooling file uploads to a caller-owned temporary file; `consumeMultipartRequestBody`
  is the thin WAI `Request` adapter. `consumeMultipartBodyWith`/`consumeMultipartRequestBodyWith` are
  incremental siblings whose callback runs as soon as each part finishes, before any later part
  (including a file part) is read, so a caller can reject the body before ever spooling a
  not-yet-reached file part to disk.
* Added `apiResponseStatus` to `ApiResponseBody` (defaulted to `200` by `apiJsonResponse`,
  `apiTextResponse`, and `apiBytesResponse`; override it with a record update, e.g. `422` for
  semantically invalid input), and `apiHttpStatus` now has standard reason phrases for `400`, `403`,
  and `422` alongside the existing `200`/`204`/`404`/`405`.
* Added `runServerWithWaiMiddleware` (composes a caller-supplied `Wai.Middleware`, such as
  `apiEndpointMiddleware`, in front of the rendered application before any runtime listener, bypassed
  only for ACME HTTP-01 challenge responses; `runServer` is now `runServerWithWaiMiddleware id`) and
  `withLocalTestServerForApplication` (serves an already-built `Wai.Application` over a real loopback
  listener, for testing such a composition; `withLocalTestServer` is now defined in terms of it).
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
