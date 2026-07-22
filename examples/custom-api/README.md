# custom-api

**Status:** Current

Show how to add app-specific API routes that are not only the default page/data flow.

Current repo alignment:

- app-owned API routes are ordinary typed `SiteRoute` values,
- `packages/web-api` exposes `/api/status` through the same route codec and response pipeline as
  page routes,
- API handlers return `BodyResponse` with their own content type, status, diagnostics, and private
  log entries.

Suggested snippet:

- [src/App/Api/Status.hs.md](src/App/Api/Status.hs.md)

Custom API handlers remain app-owned: keep their route constructor, route-codec case, and response
handler together. The framework applies request policy, middleware, response security headers,
request observability, and finalization exactly once around the resulting response.
