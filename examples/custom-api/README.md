# custom-api

**Status:** Runnable

Show how to add app-specific API routes that are not only the default page/data flow, and how the
newer, method-aware `HarchWeb.Api` declaration works end to end against a real `Wai.Application`.

## Runnable: `HarchWeb.Api` declarative dispatch

[src/App/Api/Declarative.hs](src/App/Api/Declarative.hs) is a compiled, tested module demonstrating
`HarchWeb.Api` (package `harch-web`) directly:

- `GET /api/greeting` negotiates a response from `Accept`: JSON (the default) or the
  application-defined `text/x-greeting` media type, both declared for the same endpoint.
- `POST /api/greeting` decodes a JSON request body through a hand-written `ApiBodyDecoder` (the
  extension point for a decoder shape the framework does not build in), returning `415`/`413`/`400`
  outcomes for an unsupported media type, an oversized body, or malformed JSON.
- `POST /api/avatar` consumes a `multipart/form-data` upload through `HarchWeb.Api.Multipart`.
- `apiEndpointMiddleware` dispatches all three paths; every other request passes through to whatever
  `Wai.Application` the example wraps, unchanged.

Run its tests from the repository root:

```sh
cabal test custom-api-tests --test-show-details=direct
```

`HarchWeb.Api` matches path and method independently (deriving `404`/`405 Method Not Allowed`+`Allow`/
`HEAD`/`OPTIONS` from a declared table) and is opt-in: an application wraps its existing
`Wai.Application` with `apiEndpointMiddleware` to dispatch only the paths it declares, leaving
everything else untouched. It is a fully-tested library capability, not yet the application's *default*
dispatcher — see [design guidance](../../docs/design-guidance.md#a-declarative-api-endpoint-library-exists-as-an-opt-in-wai-middleware)
for its current state.

## Implemented guide: `ApiRoute`/`RouteDefinition`

Current repo alignment for the existing, framework-default pattern:

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

Continue using this pattern for real applications unless they explicitly opt into
`apiEndpointMiddleware` for a declared set of paths.
