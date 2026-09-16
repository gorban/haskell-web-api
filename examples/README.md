# Examples

Start with a runnable application, then add one concern at a time. The labels describe what exists in
this repository today; snippets that show future API direction are kept in the explicitly labeled
design guide.

Run Cabal commands from the repository root to use the shared reviewed freeze file.
See [dependency upgrades and release provenance](../README.md#reproducible-builds-and-dependency-upgrades)
before changing example dependencies or distributing an example build.

## Executable and testable examples

These are Cabal packages with source and tests. The command column says whether
the package starts an executable or verifies a focused test suite; a test-only
package is not presented as a runnable server.

| Example | What it demonstrates | Run or verify from the repository root |
| --- | --- | --- |
| [two-pages](two-pages/README.md) | Complete SSR pages, generated page routes, typed components, an `ActionCodec` shared by typed forms and dispatch, immediate form capture, enhanced navigation, patches, and SSE. | `cabal run two-pages-example` |
| [multipart-upload](multipart-upload/README.md) | A CSRF-protected native multipart form with bounded in-memory storage, explicit upload ownership, and scripts-enabled/scripts-disabled browser proof. | `cabal run multipart-upload-example` |
| [composed-domains](composed-domains/README.md) | Composed typed modules, localized navigation, an accessible language-picker fallback, and a Help/support link with real-browser proof. | `cabal run composed-domains` |
| [localization](localization/README.md) | Application-owned ICU messages layered on framework localization primitives. | `cabal test localization-example-tests` |
| [custom-db-adapter](custom-db-adapter/README.md) | A typed, non-PostgreSQL effect interpreter with focused tests. | `cabal test custom-db-adapter-tests` |
| [custom-api](custom-api/README.md) | A method-aware `HarchWeb.Api` endpoint table (negotiated JSON/custom-media-type response, a JSON request body, a multipart upload) composed into a `Wai.Application` through the closed route-family registry. | `cabal test custom-api-tests` |

## Implemented guides

These guides point to behavior implemented and tested in the framework, full reference application,
or tracked runtime configuration.

| Guide | Add this concern |
| --- | --- |
| [PostgreSQL effects](postgres-effects/README.md) | Typed database operations, migrations, and runtime/migration identities. |
| [Custom JavaScript](custom-js/README.md) | Deferred, page-scoped enhancement on top of complete SSR. |
| [Logging and telemetry](logging-and-telemetry/README.md) | Structured logs plus OTLP traces and metrics. |
| [Testing](testing/README.md) | Unit, integration, real-browser, and 100% package coverage workflows. |
| [Provided certificates](https-provided-certificate/README.md) | HTTPS with a certificate and private-key pair. |
| [ACME / Let's Encrypt](https-acme/README.md) | Certbot-backed `http-01` issuance and shared certificate consumers. |
| [HTTPS security](https-security/README.md) | Redirects, HSTS, CSP, CORS, and response hardening. |
| [Authentication and sessions](middleware-auth-jwt/README.md) | Opaque sessions, CSRF, credentials, MFA, and protected routes. |
| [Localization](multilanguage-routing/README.md) | Locale-aware page routing and localized responses. |
| [Reverse proxy awareness](reverse-proxy-awareness/README.md) | Trusted forwarding, TLS offload, and path-prefix mounting. |

## Full-stack reference application

`packages/web-api` is the integrated reference application for account
workflows, PostgreSQL effects, telemetry, TLS configuration, localization, and
the application-owned account-activity audit. It is not a framework service or
a copy-and-run production policy. Follow [SETUP.md](../SETUP.md) for its
database, `pg_cron`, and credentials prerequisites, then use
`cabal run haskell-web-api` with the required runtime configuration. Its
integration and browser evidence is in `haskell-web-api-tests`.

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
