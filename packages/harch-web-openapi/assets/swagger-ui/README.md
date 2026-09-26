# Vendored Swagger UI assets (AHI-4E)

Pinned, reviewed third-party assets for the optional Swagger UI surface:

- `swagger-ui-bundle.js`, `swagger-ui.css` — from
  [`swagger-ui-dist`](https://www.npmjs.com/package/swagger-ui-dist) **5.33.0**
  (Apache-2.0; `LICENSE.apache-2.0` and `NOTICE` kept beside them, `package.json`
  retained as the provenance record of the exact upstream version).
- The minor version is pinned deliberately: Swagger's plugin API is not a
  stable public protocol across arbitrary versions (per the AHI-4E task
  file), so upgrades are explicit reviews, not floating `latest` fetches.

Deliberately **not** vendored from the same distribution:

- `swagger-ui-standalone-preset.js` — it injects a runtime `<style>` element,
  which the framework's default `style-src 'self'` CSP would block; the
  preset is optional chrome (topbar/standalone layout) and this surface
  renders through the typed page shell instead.
- `index.html`, `oauth2-redirect.html`, `swagger-initializer.js` — the
  typed SSR page, the package's own enhancement module, and (when the
  OAuth-panel slice lands) a package-owned redirect surface replace these.

CSP note (probe recorded 2026-09-24 against 5.33.0): the main bundle needs
neither `unsafe-eval` nor `unsafe-inline` — its single `new Function` is
webpack's dead `globalThis` polyfill (guarded by an earlier return and a
try/catch), it injects no `<style>` elements, and its inline styling goes
through CSSOM (which CSP does not block). The real-browser E2E proof in the
wiring slice asserts this under the unmodified default policy.

## Authentication assistance

The behavior module renders a small assistance panel above the mount: an
editable complete `Authorization` header value (`None` or e.g. `Bearer ey...`,
including intentionally malformed values for testing) and an optional
automatic OAuth client-credentials exchange against the application's token
endpoint. Bearer state is memory-only — the module reads no cookies and
touches no local/session storage (pinned by test) — and the client secret is
a one-use value: cleared from input, DOM, and module state after a successful
exchange, replaced by a fixed-length placeholder that reveals neither value
nor length, and announced through a polite live region. Example-mode
configuration surfaces seeded client hints as labels only, never prefills.
