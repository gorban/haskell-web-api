# custom-api

**Status:** Runnable

Show how to declare app-specific API routes with `HarchWeb.Api`'s typed endpoint boundary and
compose them into a real `Wai.Application` through the framework's closed route-family registry —
the same primitives a real application's page routes use, not a separately bolted-on dispatcher.

## Runnable: typed `HarchWeb.Api` endpoints via the route-family registry

[src/App/Api/Declarative.hs](src/App/Api/Declarative.hs) is a compiled, tested module demonstrating
`HarchWeb.Api` (package `harch-web`) end to end:

- `GET /api/greeting` negotiates a response from `Accept`: JSON (the default) or the
  application-defined `text/x-greeting` media type, both declared for the same endpoint.
- `POST /api/greeting` decodes a JSON request body through a hand-written `ApiBodyDecoder` (the
  extension point for a decoder shape the framework does not build in); the typed endpoint boundary
  itself returns `415`/`413`/`400` for an unsupported media type, an oversized body, or malformed
  JSON, before the handler ever runs.
- `POST /api/avatar` consumes a `multipart/form-data` upload through `HarchWeb.Api.Multipart`'s
  bounded in-memory storage adapter, discarding every part without promoting it.
- `declarativeApiEndpoints :: [SomeApiRouteEndpoint]` is validated once by `apiEndpointFamily`; that
  opaque value is adapted by `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` into
  one `HarchWeb.RouteCodec`/`HarchWeb.Site.RouteDefinition` pair, composed into an ordinary `HarchWeb.Site.Site` via
  `simpleSite`. The endpoint table is the sole `404`/`405`+`Allow`/`HEAD`/`OPTIONS` authority for
  every path it owns — there is no separately wrapped fallback `Wai.Application` with its own,
  potentially disagreeing, idea of what a path means. Since this example has no page routes, its
  `Site` carries a `PageShell` that no declared route ever renders; a real application supplies its
  own instead, and would combine its API family with a page family via
  `HarchWeb.Routing.combineRouteCodecs`.

Run its tests from the repository root:

```sh
cabal test custom-api-tests --test-show-details=direct
```

`HarchWeb.Api.Endpoint` matches path and method independently, deriving `404`/`405 Method Not Allowed`
+`Allow`/`HEAD`/`OPTIONS` from the declared endpoint table. The compatibility `apiEndpointMiddleware`/
`apiRouteEndpointMiddleware` helpers this example (and `examples/multipart-upload`) used before
migrating were removed once no application in this repository depended on them any longer — see
[design guidance](../../docs/design-guidance.md#follow-up-decision--standalone-family-not-found-and-the-custom-api-migration-2026-08-13)
for the migration this example made; `web-api` has since made the same move for its own
`/api/status` and `/api/second` routes.

## Implemented guide: `ApiRoute`/`RouteDefinition`

Current repo alignment for `packages/web-api`'s existing, framework-default pattern (not yet
migrated onto the route-family registry above):

- app-owned API routes are constructors in an explicit `ApiRoute` ADT,
- the app's total route dispatcher maps each constructor to a `RouteDefinition`,
- `packages/web-api` exposes `/api/status` through the same codec and response pipeline as pages, and
- API handlers return `BodyResponse` with their own content type, status, diagnostics, and private
  log entries — a capability the typed endpoint boundary above does not yet expose (see the AC
  entry in `TASKS.md`).

Suggested snippet:

- [src/App/Api/Status.hs.md](src/App/Api/Status.hs.md)

Custom API handlers remain app-owned: keep their route constructor, route-codec case, and response
definition together. The route family determines that this branch returns an API response, so there is
no separate surface flag that could conflict with it. The framework applies request policy, middleware,
response security headers, request observability, and finalization exactly once.
