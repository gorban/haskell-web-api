# Revision history for harch-web

## 0.1.2.0

* Added `HarchWeb.Api`: a declarative, method-aware `ApiEndpoint` declaration with path/method matching
  (`404`/`405`+`Allow`/`HEAD`/`OPTIONS` handling), an accumulating-error `RequestCodec` for query and
  header fields (with `apiRequestDataFromWaiRequest` to extract them from a real WAI request),
  `Content-Type`-selected buffered request-body decoding (`selectApiBodyDecoder`, with
  `jsonBodyDecoder`/`textBodyDecoder`/`bytesBodyDecoder` built-ins and `415`/`413`/`400` outcomes),
  `ResponseCodec` helpers for JSON/text/bytes bodies, RFC 9110 `Accept` header negotiation (quality
  weights, wildcards, specificity, `q=0`, and `406`), and `respondApiMatch` to render an `ApiMatchResult`
  into a transport-agnostic `ApiHttpResponse` (status, headers, and an optional body, omitted for a
  `HEAD` match). This is a standalone, fully-tested library capability; it is not yet the application's
  default request dispatcher, which remains the existing `RouteCodec`/`ApiRoute` pattern.
* Added `HarchWeb.Api.Multipart`: a bounded, incremental RFC 7578 `multipart/form-data` consumer. A pure
  boundary scanner never buffers more of a part's body than the boundary delimiter's length, so large
  file parts stream without full buffering; `Content-Disposition` field-name/filename extraction handles
  quoted values, backslash-escaped quotes, and semicolons inside quoted filenames (RFC 5987/6266 extended
  `filename*=` parameters are not supported). `consumeMultipartBody` drives the scanner against any
  chunked `IO ByteString` source, enforcing per-field, per-file, and part-count limits, keeping field
  values in memory and spooling file uploads to a caller-owned temporary file; `consumeMultipartRequestBody`
  is the thin WAI `Request` adapter.
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
