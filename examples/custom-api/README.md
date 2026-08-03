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
