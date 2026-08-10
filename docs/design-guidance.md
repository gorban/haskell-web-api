# Design guidance

This document does two things. "Design decisions before you build" (below) is the decision
framework a Large, foundational, or security-critical task must apply *before* implementation
begins, with its chosen decisions recorded, not picked silently. Everything after "Landed
conventions" separates Harch Web's landed authoring model from intentionally future-facing
design, describing what has already shipped. For the architecture overview, begin with the
[root README](../README.md). For executable source, begin with the
[two-pages](../examples/two-pages/README.md) guide.

## Design decisions before you build

Apply this section before starting a task scoped Large, foundational, or security-critical, and
again at any point during implementation where one of its questions becomes live. Record which
choice was made and why — in the touched module's Haddock and, when it changes what an area of
the framework can do, in this document's status table below — before the change lands. A decision
made but not written down is indistinguishable, to the next reader, from a decision never made.

### Extend an existing boundary before adding a parallel one

Before adding a new dispatch, routing, codec, or protocol abstraction, check whether an existing
one already owns that responsibility — `RouteDefinition`/`RouteCodec` for path/method dispatch,
`ActionCodec` for typed submissions, `Response`/`ResponseBody` for what the server can return.
Default to extending that existing type or its interpreter. Add a second, additive abstraction
that coexists with the first only when at least one of the following holds, and record which one:

- the existing abstraction's tests, name, or documented contract would have to change meaning for
  existing callers to accommodate the new behavior, and an additive layer avoids that breaking
  change to a stable public contract;
- the new abstraction covers a genuinely disjoint concern the existing type deliberately does not
  model (a byte-stream body consumer is not a route dispatcher); or
- the task's own text explicitly authorizes a new, separate surface instead of extension.

A task description that merely *offers* extension as one possible shape ("replace or extend the
surrounding `X`") defaults to extension unless one of the three conditions above is met and
written down — it is not itself authorization for a parallel surface.

**Worked example.** A declarative API endpoint layer was built as a fully separate, additive WAI
middleware, coexisting with the framework's existing page/action dispatch contract, even though
the task that requested it explicitly offered extending that existing dispatch contract as an
option. That phrasing alone does not satisfy the third condition above (explicit authorization for
a *new* surface) — the default was to extend the existing dispatch contract into a method-aware
one, which a follow-up task then had to require anyway. Two dispatchers now have to be kept in
sync by hand instead of one contract owning path/method ownership outright.

### When implementation hits a missing framework capability

If finishing a task requires a capability the framework does not yet expose — for example, a
response type that cannot set a header the task needs — stop and choose one of the following, in
this order of preference, and write the choice and a one-paragraph rationale into the commit
message and this document before continuing:

1. **Add the primitive to the framework** when the gap is small, general, and squarely within an
   area the framework already owns (see "Framework and application ownership" below). This is
   almost always the right choice when every application of that kind will eventually need it.
2. **Work around it in the application layer** only when the gap is application-specific, the
   framework change would be speculative or oversized for this task's scope, and the workaround
   does not silently substitute for a security property the requested feature depends on. Do not
   replace a mechanism the task assumed (e.g., a cookie-based scheme) with a workaround that
   changes its guarantees without naming that change explicitly.
3. **Flag and stop** when neither of the above is safe to decide unilaterally mid-task — in
   particular when the workaround would ship a materially different security or correctness
   property than the task assumed. Leave the gap and the blocked decision in the handoff instead
   of picking silently and moving on.

Never make this choice invisibly inside an unrelated commit. A reviewer must be able to find the
decision without reconstructing it from an implementation diff.

**Worked example.** A native file-upload form needed to set a `Set-Cookie` header from a plain page
response for a double-submit CSRF cookie, and the response type had no header field at all. The gap
was worked around silently: a single-use, server-held token replaced the cookie-based scheme the
task assumed, with a materially different security property (no cross-tab support, no expiry) and
no rationale recorded anywhere a reviewer would see it before it shipped. This was option 3
territory (flag and stop), or at minimum option 1 with a small, general response-header field — not
a silent option-2 substitution of the mechanism the task assumed.

### Untrusted input gets an explicit ownership and storage boundary

Any feature that ingests untrusted request data into a durable or bounded resource — a file, a
buffer, a database row, a queue entry, a session slot — must define, before implementation, who
owns that resource across its lifecycle: which caller-supplied or framework-chosen backend
receives it, how it is bounded, and what discards it on rejection, exception, disconnect, or
cancellation. Do not default to a hardcoded backend (a local temp file, an unbounded in-process
buffer, an implicit table) as the framework's only or implicit policy. Model the resource behind
an explicit, application-suppliable adapter with a staged-then-promoted-or-discarded lifecycle,
and never hand the caller a raw handle into a framework-internal storage decision (a temp-file
path) as if it were that adapter. This applies beyond file uploads to any future feature accepting
untrusted payloads — streamed request bodies, WebSocket frames, deferred job payloads, and similar.

**Worked example.** A multipart request-body consumer that spools every file part straight to a
local temporary file and hands the caller that raw path has a hardcoded backend, no bounded
in-memory option, and no explicit promote-or-discard step the caller controls. This is exactly
the shape this rule exists to prevent. `HarchWeb.Api.Multipart` now uses an explicit storage
adapter and a bounded in-memory default. Its WAI request wrappers now parse a strict,
case-insensitive @multipart/form-data@ media type with exactly one valid boundary and reject an
oversized declared @Content-Length@ before they call the body reader. The scoped streaming API
exposes completed file parts as opaque claims: application code either promotes a claim once or
discards it, and the framework discards every unclaimed upload when the callback scope exits.
The earlier raw collector remains a compatibility path to retire under AD.

### Naming a partial slice in the status table

A row in "Current capability and remaining design direction" may say `Implemented` only when the
shipped surface matches its full designed scope. When a Large task's MVP slice ships ahead of that
scope, use `Implemented (partial — see <task id>)` and name the concrete gap and the tracked
follow-up in the Guidance column. Never let a status-table row imply a wider capability than what
shipped; a missing follow-up reference next to a partial `Implemented` is itself a defect in this
table, fixed the same way any other stale documentation is fixed.

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
it synthesizes), and a match is `ApiRouteMatched`/`ApiRouteMatchedHead`/`ApiRouteOptions`. `HEAD` is never
declared directly — a matched `GET` endpoint answers it with the same target and no body — and neither is
`OPTIONS`: any request method `OPTIONS` against a path with at least one declared endpoint synthesizes
`ApiRouteOptions`, which `respondApiMatch`/`apiEndpointMiddleware` render as `204 No Content` with the same
`Allow` header and no body, without running any endpoint's handler. `HarchWeb.Api` does not implement CORS:
it never reads `Origin`/`Access-Control-Request-Method` and never emits an `Access-Control-*` header. An
application that needs CORS preflight support composes its own middleware in front of
`apiEndpointMiddleware` (typically intercepting `OPTIONS` itself before it reaches this table), keeping
that policy application-owned rather than baked into every endpoint declaration. `RequestCodec` decodes query and
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
omitted for a `HEAD` match. A target's `ApiResponseBody` carries its own `apiResponseStatus`, defaulted
to `200` by `apiJsonResponse`/`apiTextResponse`/`apiBytesResponse` and overridden with a record update
for an ordinary typed non-200 outcome, e.g. `422` for input that decodes but fails semantic validation;
`apiHttpStatus` has standard reason phrases for `200`, `204`, `400`, `403`, `404`, `405`, and `422`.
`apiHttpResponseToWaiResponse` renders that into a real WAI `Response`, and
`apiEndpointMiddleware` is the integration point: a `Wai.Middleware` an application opts into by wrapping
its own `Wai.Application`, dispatching a request whose path matches a declared endpoint and falling
through to the wrapped application for everything else. Adopting it is purely additive — it composes
independently of whatever dispatcher (page routes, client actions, or anything else) the wrapped
application already has, changing behavior only for paths explicitly declared as `ApiEndpoint`s.

`HarchWeb.Api.Multipart` adds a bounded, incremental RFC 7578 consumer: a pure boundary scanner
(`newMultipartScanner`/`feedMultipartChunk`/`finishMultipartScanner`) that never retains more of a part's
body than the boundary delimiter's length, `parseMultipartFieldDisposition` for a part's `name`/`filename`,
and `consumeMultipartBody`/`consumeMultipartRequestBody` to drive the scanner against a chunked body
(any `IO ByteString` source, or a WAI `Request` directly). `MultipartStorage` makes durable backends an
explicit application choice; the WAI helpers use the supplied `InMemoryUpload` adapter, retaining no file
bytes beyond `MultipartLimits`' max-file budget. `consumeMultipartBodyWith` and
`consumeMultipartRequestBodyWith` are the incremental siblings: a caller-supplied callback runs as soon
as each part finishes, before any later part (including a later file part) is read, so a caller can reject
the whole body — on an invalid CSRF field, say — before a later file reaches storage. `consumeMultipartBody`
is a thin wrapper that always accepts and accumulates. Cleanup after later rejection, exceptions, or aborts,
and explicit durable promotion, remain partial work under AD.

Both modules are implemented and fully unit-tested; `apiEndpointMiddleware` makes `ApiEndpoint` dispatch
usable against a real WAI application today, opt-in and additive. Neither is the application's
*default* dispatcher: continue routing real applications through the existing `RouteCodec`/`ApiRoute`
pattern shown in the [custom API guide](../examples/custom-api/README.md) unless they explicitly opt
into `apiEndpointMiddleware` for a declared set of paths.

`runServerWithWaiMiddleware`/`withLocalTestServerForApplication` are the composition points that make
`apiEndpointMiddleware` usable against a real running (or locally test-served) application, not just a
bare `Wai.Application` in a unit test: `runServer` and `withLocalTestServer` are now defined as the `id`
middleware case of each. [two-pages](../examples/two-pages/README.md)'s `/native-upload` page
(`App.NativeUpload`) is the compiled, tested demonstration of the whole native-upload slice: a plain
`<form enctype="multipart/form-data">` with no `data-harch-action` attribute (so the inline capture
kernel's `form[data-harch-action="true"]` selector never matches it and the browser submits it
natively, with or without JavaScript), a single-use server-held CSRF token embedded as a hidden field
(chosen over a double-submit cookie because nothing in this framework version lets a plain page response
set a `Set-Cookie` header), and `consumeMultipartRequestBodyWith` validating that field via its
per-part callback — before any later part, including the file part, is read — so a request whose file
part follows an invalid or absent CSRF field is rejected before that file reaches storage. See
the module haddock in `examples/two-pages/src/App/NativeUpload.hs` for the full policy, and
`test/E2E/AppSpec.hs`'s two native-upload scenarios (scripts enabled and disabled) for the real-browser
proof that the submission is a hard navigation with zero capture-kernel mutation requests either way.

## Current capability and remaining design direction

Every row's `State` follows the "Naming a partial slice" convention above: `Implemented` means
the full designed scope shipped; a partial slice must say so and name its follow-up.

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
| `HarchWeb.Api.Multipart` bounded streaming consumer, in-memory default, and native upload form | Implemented (partial — see AD) | Durable storage selection is explicit; complete staged-upload cleanup, promotion, and parser-wide bounds remain under AD. See [two-pages](../examples/two-pages/README.md)'s `/native-upload` page (`App.NativeUpload`) for the compiled, real-browser-tested demonstration. |
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
