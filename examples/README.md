# Examples

Start with a runnable application, then add one concern at a time. The labels describe what exists in
this repository today; snippets that show future API direction are kept in the explicitly labeled
design guide.

## Runnable examples

These are Cabal packages with executable source and tests.

| Example | What it demonstrates | Run from the repository root |
| --- | --- | --- |
| [two-pages](two-pages/README.md) | Complete SSR pages, generated page routes, typed components, immediate form capture, enhanced navigation, patches, and SSE. | `cabal run two-pages-example` |
| [custom-db-adapter](custom-db-adapter/README.md) | A typed, non-PostgreSQL effect interpreter with focused tests. | `cabal test custom-db-adapter-tests` |

## Implemented guides

These guides point to behavior implemented and tested in the framework, full reference application,
or tracked runtime configuration.

| Guide | Add this concern |
| --- | --- |
| [PostgreSQL effects](postgres-effects/README.md) | Typed database operations, migrations, and runtime/migration identities. |
| [Custom JavaScript](custom-js/README.md) | Deferred, page-scoped enhancement on top of complete SSR. |
| [Custom API](custom-api/README.md) | An explicit typed API route and `RouteDefinition`. |
| [Logging and telemetry](logging-and-telemetry/README.md) | Structured logs plus OTLP traces and metrics. |
| [Testing](testing/README.md) | Unit, integration, real-browser, and 100% package coverage workflows. |
| [Provided certificates](https-provided-certificate/README.md) | HTTPS with a certificate and private-key pair. |
| [ACME / Let's Encrypt](https-acme/README.md) | Certbot-backed `http-01` issuance and shared certificate consumers. |
| [HTTPS security](https-security/README.md) | Redirects, HSTS, CSP, CORS, and response hardening. |
| [Authentication and sessions](middleware-auth-jwt/README.md) | Opaque sessions, CSRF, credentials, MFA, and protected routes. |
| [Localization](multilanguage-routing/README.md) | Locale-aware page routing and localized responses. |
| [Reverse proxy awareness](reverse-proxy-awareness/README.md) | Trusted forwarding, TLS offload, and path-prefix mounting. |

## Workflow guide

- [Local HTTPS with mkcert](https-mkcert/README.md) is a practical certificate-generation workflow
  layered onto the implemented manual-certificate listener. It is a workflow, not a separate framework
  API.

## Design direction

- [Route templates](route-templates/README.md) describes the intended dynamic path/query DSL. Dynamic
  paths and query parsing are already possible as explicit typed routes, but this declarative template
  syntax is not executable yet.

## Choosing a starting point

Use `two-pages` to learn the architecture without a database, telemetry collector, TLS setup, or reverse
proxy. Add the focused guides as the application needs them. `packages/web-api` remains the combined,
full-stack reference when you need to see all of the seams wired together.

The current framework conventions and the boundary between landed behavior and future design live in
[design guidance](../docs/design-guidance.md). Runtime environment variables are centralized in
[runtime configuration](../docs/runtime-configuration.md).
