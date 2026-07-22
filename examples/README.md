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
| [two-pages](two-pages/README.md) | Working | The first thing a new user should copy: two SSR pages plus progressive enhancement. |
| [postgres-effects](postgres-effects/README.md) | Current | Add a real PostgreSQL-backed effect and the migration environment needed for it. |
| [custom-db-adapter](custom-db-adapter/README.md) | Working | Build and test a typed non-PostgreSQL database interpreter. |
| [logging-and-telemetry](logging-and-telemetry/README.md) | Current | Turn on logs/traces locally and show how to inspect them. |
| [testing](testing/README.md) | Current | Show app-level unit, integration, e2e, and coverage workflows. |
| [https-provided-certificate](https-provided-certificate/README.md) | Current | Run HTTPS directly with a provided certificate/key pair. |
| [https-mkcert](https-mkcert/README.md) | Iterative | Show the local-dev certificate workflow we want, using mkcert-style steps. |
| [https-acme](https-acme/README.md) | Current | Explain ACME/certbot-backed real certificate flow and its constraints. |
| [https-security](https-security/README.md) | Current | Enable redirects, HSTS, and related browser hardening settings. |
| [middleware-auth-jwt](middleware-auth-jwt/README.md) | Working | Use opaque sessions, CSRF, and the account profile flow for protected surfaces. |
| [custom-js](custom-js/README.md) | Iterative | Add a page-specific browser behavior without giving up SSR-first rendering. |
| [custom-api](custom-api/README.md) | Iterative | Add app-specific API routes beyond the normal page/data flow. |
| [multilanguage-routing](multilanguage-routing/README.md) | Iterative | Document the target path-based i18n routing story. |
| [reverse-proxy-awareness](reverse-proxy-awareness/README.md) | Current | Run behind nginx with trusted forwarding and optional subpath mounting. |

## Next Priority

Before continuing the topic-ordered catalog, define the route-template example shape in
[route-templates](route-templates/README.md). It should cover both path parameters such as
`/posts/:slug` and query-string parameters such as `/search?q=...`, including how `harch` anchors
and GET forms parameterize those routes without hand-built URLs.

## Notes

- `packages/web-api` remains the combined, full-featured example app.
- The example folders here are intentionally smaller and cleaner than `packages/web-api`.
- The snippet files are Markdown on purpose. They are intended to communicate the desired file shape
  and authoring model even when the current framework still needs refinement.
