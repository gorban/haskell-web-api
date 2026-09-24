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