# Design guidance

This document separates Harch Web's landed authoring model from intentionally future-facing design.
For the architecture overview, begin with the [root README](../README.md). For executable source, begin
with [two-pages](../examples/two-pages/README.md).

## Landed conventions

### Complete SSR is the baseline

Every supported page route renders a complete document for direct loads, reloads, crawlers, and
scripts-disabled navigation. JavaScript enhances declared surfaces; it does not define whether the page
exists. Native links remain navigable, and a modeled framework control must have an immediate capture
path before it is shipped enabled; a native fallback is an explicit action capability, not a default.

### Apps have a small composition root

The runnable starter is organized as:

```text
app/Main.hs
SetupHooks.hs
src/App/
  App.hs
  Routes.hs
  Pages/
    Home.hs
    LiveData.hs
    NotFound.hs
    Second.hs
  CustomPages/
    Preview.hs
  Components/
    Layout.hs
    ExampleAuthor.hs
    SubscriptionEmailField.hs
public/
  navigation.js
  live-data.js
  two-pages.css
```

`App.App` wires the route codec, total dispatcher, layout, actions, assets, and policy. Each discovered
page module exports a `pageDefinition`. `App.Routes` owns explicit API and dynamic route parsing.

### Folder-driven page generation is real

The two-pages Cabal setup hook discovers `App.Pages.*` modules and invokes the
`harch-page-routes` build tool. Generated modules provide:

- the closed `PageRoute` ADT,
- `allPageRoutes`,
- total page path rendering and parsing, and
- an exhaustive `pageRouteDefinition` that references every discovered module.

Static page registration therefore cannot drift from the discovered module set. Dynamic paths, query
decoding, APIs, and app-specific custom pages stay explicit instead of being forced into the static-page
convention.

### Components are typed Haskell functions

Use the `harch` quasiquoter for page and component bodies. Component calls support nullary records,
named record fields, computed expressions, nested or computed children, and an explicit positional
fallback for multiple distinct typed arguments. The syntax lowers to ordinary Haskell calls and the
escaping-by-default `Html` AST.

Prefer:

- named fields for cohesive component input records,
- opaque newtypes or ADTs instead of ambiguous primitive arguments,
- nested markup for ordinary children,
- `children={value}` when children are computed, and
- `props={[first, second]}` only for an intentionally positional multi-argument function.

Scoped class names are available through `cssScope` and `ScopedCssClass`. App styles remain ordinary
static assets today; a typed CSS or JavaScript asset EDSL has not landed.

### Route families encode capabilities

Apps compose generated pages with explicit APIs and custom paths using a sum such as:

```hs
data AppRoute
  = Page PageRoute
  | Api ApiRoute
  | Custom CustomRoute
```

The constructor determines the response branch. Do not reintroduce a separate surface flag that could
represent impossible combinations. Keep path and query validation pure and make parsed domain values
opaque where practical.

### Framework and application ownership

The framework owns:

- the `Html` AST, centralized escaping, document rendering, and CSP nonces,
- page route generation and the shared site dispatcher,
- native/deferred navigation conventions and the immediate event-capture contract,
- typed client-action responses, region replacement, and SSE transport,
- server integration, request policy, security headers, proxy prefixes, and observability seams, and
- accessible live-region helpers and browser-test infrastructure.

The application owns:

- domain route values and explicit dynamic/API parsing,
- page models, components, branding, and assets,
- domain effects and concrete adapters,
- authentication/authorization policy and custom middleware, and
- custom API payloads and handlers.

### Client-action codecs own both sides of a submission

Declare each modeled client action once with `HarchWeb.Action.ActionCodec target context action`. Its
endpoint declaration owns the typed action target, HTTP method, context-aware path printer, and
applicative decoder. Render framework forms with `HarchWeb.Controls.actionForm` from that codec; do not
repeat action paths, methods, or application-owned field lookups in the component and server layers.
The codec rejects duplicate method/path identities at construction.

Server dispatch first matches the rendered path and declared method, then runs only that endpoint's
decoder. An unknown path is `404`; a known path with no declared method is `405` with `Allow`; malformed
matched fields are `400`; and a successful decode stays on the application rail, where expected domain
validation can return the existing localized `422` region patch. Independent fields use the accumulating
applicative so missing, duplicate, and syntactically invalid fields are reported in declaration order.
Those parse errors expose only stable constructors and field names—never submitted values. Keep business
validation separate from this protocol parsing boundary.

### Captured actions have local, bounded ownership

The inline capture kernel owns a captured action in memory until a deferred handler confirms completion,
reports a visible recoverable outcome, or the user cancels it. It records the input snapshot only for that
live document, associates it with the originating control, and updates the control-local `status` region;
it does not log submitted values, persist them in browser storage, run application behavior, transport
actions, or patch regions. Deferred modules register generic handlers, claim matching actions without
removing them, and settle with a claim identity so stale or duplicate consumers cannot settle another
handler's work.

Pending feedback is immediate. A liveness threshold may change it to a visible delayed state, but time is
not evidence that loading failed and never triggers submit, retry, cancellation, or ownership transfer.
Synchronous exceptions, rejected promises, and module-load failures become local recoverable states.
This proves ownership after capture only: it does not promise eventual handler execution, delivery across
navigation/reload, survival of tab or process termination, or cross-device durability.

`ActionFormAttributes` declares capabilities rather than making framework-wide promises. The default is an
exclusive client handler. Native submission is explicit because client-only effects may have no compatible
server/CSRF endpoint. `NativeFallback` requires a `NativeActionFallback` value with that endpoint, method,
and the server workflow's CSRF form value; enhanced dispatch remains on the typed action codec path.
Conditional leave confirmation installs `beforeunload` only while an unresolved
eligible action exists, and is a best-effort warning. Retry for an indeterminate mutation is safe only
with the same idempotency identity and a server deduplication boundary; otherwise keep the action visibly
recoverable. `HandlerSafeRetry` exposes a local retry control only after a recoverable handler outcome;
it reclaims the retained envelope without automatic replay. `IdempotentMutationRetry` requires an
`ActionIdempotency` value and forwards the same key with every retry to the typed server action request,
whose application handler supplies the durable deduplication boundary. See the real-browser lifecycle
proofs—delayed registration, cancellation, handler failure and non-settlement, script failure, multiple
controls, conditional leave warning, and safe/idempotent retry—in
[two-pages](../examples/two-pages/test/E2E/AppSpec.hs).

### A declarative API endpoint library exists as an opt-in WAI middleware

`HarchWeb.Api` declares a method-aware HTTP endpoint (`apiEndpoint target method (at "/path")`) and
matches a request against a table of them with `matchApiEndpoints`: an unmatched path is
`NoApiRouteMatch` (`404`), a matched path with no accepting method is `ApiMethodNotAllowed` (`405`, with
`apiAllowHeaderValue` rendering `Allow` from every declared method at that path plus the `HEAD`/`OPTIONS`
it synthesizes), and a match is `ApiRouteMatched`/`ApiRouteMatchedHead`. `RequestCodec` decodes query and
header fields applicatively via `requiredField`/`optionalField`/`fieldWithDefault`, accumulating every
independent `MissingApiField`/`DuplicateApiField`/`InvalidApiField` rather than stopping at the first one,
matching `HarchWeb.Action`'s decoder shape; `apiRequestDataFromWaiRequest` extracts the `ApiRequestData`
a `RequestCodec` runs against from a real WAI request. `selectApiBodyDecoder` selects a declared, fully-buffered
request-body decoder (`jsonBodyDecoder`, `textBodyDecoder`, `bytesBodyDecoder`, or an application-defined
`ApiBodyDecoder`) by the request's `Content-Type` (ignoring its parameters, case-insensitively),
enforces a byte limit before decoding, and distinguishes an unsupported media type (`415`), an oversized
body (`413`), and a malformed body (`400`) from a successful decode; `MissingContentTypePolicy` lets each
endpoint decide whether a missing header is rejected or resolved to a declared default.
`selectRepresentation` negotiates a response media type from a server-preference-ordered list and an
optional `Accept` header per RFC 9110 §12.5.1 (quality weights, wildcards, specificity precedence, `q=0`
exclusion, `406` when nothing is acceptable). `respondApiMatch` renders an `ApiMatchResult` into a
transport-agnostic `ApiHttpResponse` (status, headers, optional body): `404`/`405`+`Allow` for the two
non-matching outcomes, and the matched target's rendered status/`Content-Type` otherwise, with the body
omitted for a `HEAD` match. `apiHttpResponseToWaiResponse` renders that into a real WAI `Response`, and
`apiEndpointMiddleware` is the integration point: a `Wai.Middleware` an application opts into by wrapping
its own `Wai.Application`, dispatching a request whose path matches a declared endpoint and falling
through to the wrapped application for everything else. Adopting it is purely additive — it composes
independently of whatever dispatcher (page routes, client actions, or anything else) the wrapped
application already has, changing behavior only for paths explicitly declared as `ApiEndpoint`s.

`HarchWeb.Api.Multipart` adds a bounded, incremental RFC 7578 consumer: a pure boundary scanner
(`newMultipartScanner`/`feedMultipartChunk`/`finishMultipartScanner`) that never retains more of a part's
body than the boundary delimiter's length, `parseMultipartFieldDisposition` for a part's `name`/`filename`,
and `consumeMultipartBody`/`consumeMultipartRequestBody` to drive the scanner against a chunked body
(any `IO ByteString` source, or a WAI `Request` directly), enforcing `MultipartLimits` (max field bytes,
max file bytes, max part count) and spooling file parts to a caller-owned temporary file rather than
buffering them.

Both modules are implemented and fully unit-tested; `apiEndpointMiddleware` makes `ApiEndpoint` dispatch
usable against a real WAI application today, opt-in and additive. Neither is yet the application's
*default* dispatcher, and native multipart form upload with CSRF/AA capture coordination is not built:
continue routing real applications through the existing `RouteCodec`/`ApiRoute` pattern shown in the
[custom API guide](../examples/custom-api/README.md) unless they explicitly opt into
`apiEndpointMiddleware` for a declared set of paths.

## Current capability and remaining design direction

| Area | State | Guidance |
| --- | --- | --- |
| Complete SSR and enhanced navigation | Implemented | Keep direct loads and scripts-disabled behavior in every page test. |
| Immediate modeled-form capture | Implemented | Extend the kernel contract before adding another enabled framework event type. |
| Generated static page algebra/dispatch | Implemented | Export `pageDefinition`; keep API and dynamic routes explicit. |
| Typed markup and component calls | Implemented | Prefer named record fields; reserve positional `props` for distinct typed values. |
| Scoped CSS names | Implemented | Use `cssScope`; typed CSS authoring remains future work. |
| Declarative client actions and region patches | Implemented | Declare `ActionCodec` endpoints once; render forms and dispatch from it, then mutate with typed action responses and `RegionPatch`, not page POST/reload workflows. |
| SSE live updates | Implemented | Start from meaningful SSR content; treat streaming as an enhancement. |
| PostgreSQL and custom adapters | Implemented | Keep operations typed and interpreters app-selectable. |
| Auth, sessions, MFA, localization, telemetry, TLS, and proxy support | Implemented | Use the focused guides and full reference app. |
| `HarchWeb.Api` endpoint matching, codecs, negotiation, and `apiEndpointMiddleware` | Implemented, opt-in via WAI middleware | Wrap a `Wai.Application` with `apiEndpointMiddleware` to dispatch declared paths; it is not the default dispatcher, so keep routing everything else through `RouteCodec`/`ApiRoute`. |
| `HarchWeb.Api.Multipart` bounded streaming consumer | Implemented, not yet wired into native forms | Drive it directly from a WAI request today; native upload-form/CSRF/AA capture coordination is not built yet. |
| Declarative dynamic path/query templates | Design direction | Use explicit typed codecs until the route-template DSL is executable. |
| Typed page-local CSS/JavaScript EDSLs | Design direction | Keep current assets narrow, deferred, and route-aware by convention. |
| Automatic database-to-live-view subscriptions | Design direction | Use explicit SSE today; do not imply automatic subscriptions exist. |

The design-only route syntax is isolated in the
[route-templates guide](../examples/route-templates/README.md). It must remain labeled as design
direction until its path/query DSL is executable and tested.

## Example taxonomy

The [examples index](../examples/README.md) uses four labels:

- **Runnable:** a Cabal package with executable source and tests.
- **Implemented guide:** documentation grounded in tested framework or reference-app behavior.
- **Workflow guide:** operational steps layered on an implemented API.
- **Design direction:** a desired API shape that is not executable yet.

Do not mix proposed names into an implemented guide without labeling the specific snippet. Prefer links
to compiled source for landed behavior.

## Accessibility and verification

Treat every complete document and every region patch as an accessibility surface:

- use semantic landmarks, headings, labels, and native controls before ARIA,
- give each control an accessible name and associate validation with its field,
- use assertive alerts for actionable errors and polite status for independent progress,
- restore focus deliberately after a patch, and
- verify keyboard interaction, direct loads, reloads, early interaction, and scripts-disabled fallback
  with real-browser tests.

Unit tests should prove route totals, codecs, escaping, and action results. Browser tests should prove
the architectural timing invariants that unit tests cannot: a module may be delayed, but a visible
framework control must not lose the user's event or entered value.
