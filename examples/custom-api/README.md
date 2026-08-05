# custom-api

**Status:** Implemented guide

Show how to add app-specific API routes that are not only the default page/data flow.

Current repo alignment:

- app-owned API routes are constructors in an explicit `ApiRoute` ADT,
- the app's total route dispatcher maps each constructor to a `RouteDefinition`,
- `packages/web-api` exposes `/api/status` through the same codec and response pipeline as pages, and
- API handlers return `BodyResponse` with their own content type, status, diagnostics, and private
  log entries.

Suggested snippet:

- [src/App/Api/Status.hs.md](src/App/Api/Status.hs.md)

Custom API handlers remain app-owned: keep their route constructor, route-codec case, and response
definition together. The route family determines that this branch returns an API response, so there is
no separate surface flag that could conflict with it. The framework applies request policy, middleware,
response security headers, request observability, and finalization exactly once.

`HarchWeb.Api` (package `harch-web`) is a newer, method-aware alternative for declaring endpoints: it
matches path and method independently (deriving `404`/`405 Method Not Allowed`+`Allow`/`HEAD`/`OPTIONS`
from a declared table), decodes query/header fields and a `Content-Type`-selected buffered request body
with accumulating errors, and negotiates a response representation from `Accept`. It is a standalone,
fully-tested library capability today, not yet the application's default request dispatcher — see
[design guidance](../../docs/design-guidance.md#a-declarative-api-endpoint-library-exists-ahead-of-dispatch-wiring)
for its current state and `HarchWeb.Api.Multipart` for a bounded, incremental multipart/form-data body
consumer. Continue using the `ApiRoute`/`RouteDefinition` pattern above for real applications until it is
wired in.
