# Examples

These examples define the desired "start small, add one feature at a time" story for `harch-web`.

The repository already has a combined app under `packages/web-api`, plus runtime-focused materials in
`examples/runtime-config/` and `examples/reverse-proxy/`. This new catalog adds isolated,
docs-first examples so someone can build up their own site feature by feature instead of starting
from the full combined app immediately.

The design target and alignment analysis live in [../docs/design-guidance.md](../docs/design-guidance.md).

## Status labels

- **Current**: grounded in behavior already present in the repo.
- **Iterative**: document it now, while being honest about rough edges that should later move into
  the framework.
- **Aspirational**: a desired file shape and workflow that still needs framework work.

## Recommended order

| Example | Status | Purpose |
| --- | --- | --- |
| [01-two-pages](01-two-pages/README.md) | Iterative | The first thing a new user should copy: two SSR pages plus progressive enhancement. |
| [02-postgres-effects](02-postgres-effects/README.md) | Current | Add a real PostgreSQL-backed effect and the migration environment needed for it. |
| [03-custom-db-adapter](03-custom-db-adapter/README.md) | Iterative | Show the desired seam for swapping in another database technology. |
| [04-logging-and-telemetry](04-logging-and-telemetry/README.md) | Current | Turn on logs/traces locally and show how to inspect them. |
| [05-testing](05-testing/README.md) | Current | Show app-level unit, integration, e2e, and coverage workflows. |
| [06-https-provided-certificate](06-https-provided-certificate/README.md) | Current | Run HTTPS directly with a provided certificate/key pair. |
| [07-https-mkcert](07-https-mkcert/README.md) | Iterative | Show the local-dev certificate workflow we want, using mkcert-style steps. |
| [08-https-acme](08-https-acme/README.md) | Current | Explain ACME/certbot-backed real certificate flow and its constraints. |
| [09-https-security](09-https-security/README.md) | Current | Enable redirects, HSTS, and related browser hardening settings. |
| [10-middleware-auth-jwt](10-middleware-auth-jwt/README.md) | Aspirational | Show the desired middleware/login/JWT-cookie shape for a protected page. |
| [11-custom-js](11-custom-js/README.md) | Iterative | Add a page-specific browser behavior without giving up SSR-first rendering. |
| [12-custom-api](12-custom-api/README.md) | Iterative | Add app-specific API routes beyond the normal page/data flow. |
| [13-multilanguage-routing](13-multilanguage-routing/README.md) | Iterative | Document the target path-based i18n routing story. |
| [14-reverse-proxy-awareness](14-reverse-proxy-awareness/README.md) | Current | Run behind nginx with trusted forwarding and optional subpath mounting. |

## Notes

- `packages/web-api` remains the combined, full-featured example app.
- The example folders here are intentionally smaller and cleaner than `packages/web-api`.
- The snippet files are Markdown on purpose. They are intended to communicate the desired file shape
  and authoring model even when the current framework still needs refinement.
