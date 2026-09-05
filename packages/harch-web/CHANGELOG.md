# Revision history for harch-web

## 0.1.2.0

* **Breaking: substantial public framework API reorganization.** Affected: application/site composition; routing and typed API/action declarations; server/runtime/transport setup; security and markup types; ACME, Google Workspace, and OTLP integration.
* **Breaking:** Removed the legacy API middleware/compatibility dispatch surface; typed endpoint tables now compose through `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` and `HarchWeb.Site`.
* (added) Declarative SSR sites, typed client actions, context-aware API route definitions and closed route families with explicit method policies, negotiated and streaming request/response codecs, and private endpoint-response diagnostics.
* (added) Bounded URL-encoded and multipart streaming, including field/file-count budgets and scoped upload ownership.
* (added) Explicit interaction capture/native fallback, concurrent-request admission, typed resource ownership, and real-server/browser test seams.
* (added) Security hardening for request-head and transport limits (standard `400`/`413`/`414`/`431` rejections), forwarded headers, redirects, markup URLs/attributes, credentials, TOTP, sessions, and secret handling.
* (added) Explicit ACME webroot, HTTP-manager, and token-cache dependencies; obsolete ACME client/parser compatibility APIs were removed.

## 0.1.1.0

* Initial stub facade package for the SSR framework boundary.
* Added the first public facade types and functions: `Application`, `Page`, `RouteCodec`, `application`, `matchRoute`, and `runServer`.
* Extended the facade to carry routed request context and to distinguish page responses from future API/data responses.
* Added package-local unit tests for route matching and the stub server boundary.
* Wired the package into first-party repository conventions with lowercase package naming and `Custom` setup that copies the root `LICENSE` file.
