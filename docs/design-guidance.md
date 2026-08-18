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
There is no public raw collector: all multipart consumers use scoped ownership.

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

Every complete HTML response receives one fresh opaque CSP nonce from the framework's cryptographic
entropy source. The server passes that same nonce to document rendering, CSP construction, and the
CSRF cookie; non-page responses carry no nonce. `renderDocumentForTests` is intentionally deterministic
and must only support golden markup tests—production callers use `renderDocumentWithNonce` with a
freshly generated nonce, so no stable nonce can become a valid script capability.

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
- server integration, request policy, security headers, proxy prefixes, observability seams, and the
  private shared constant-work comparison primitive used by framework credential boundaries, and
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

### API transport helpers and the shared endpoint boundary

`HarchWeb.Api`'s `apiRouteEndpoint`/`apiRouteEndpointAt` declare a typed endpoint: one method, a typed
query/header/cookie decoder, either no body or exactly one bounded buffered/streaming/multipart body
consumer, a domain-failure interpreter, and the response renderer. `RequestCodec` decodes query and
header fields applicatively via `requiredField`/`optionalField`/`fieldWithDefault`, accumulating every
independent `MissingApiField`/`DuplicateApiField`/`InvalidApiField` rather than stopping at the first one,
matching `HarchWeb.Action`'s decoder shape; `apiRequestDataFromWaiRequest` extracts the `ApiRequestData`
a `RequestCodec` runs against from a real WAI request. `selectApiBodyDecoder` selects a declared, fully-buffered
request-body decoder (`jsonBodyDecoder`, `textBodyDecoder`, `bytesBodyDecoder`, or an application-defined
`ApiBodyDecoder`) by the request's `Content-Type` (ignoring its parameters, case-insensitively), and
distinguishes an unsupported media type (`415`) and a malformed body (`400`) from a successful decode. The
endpoint's bounded body reader owns the separate oversized-body (`413`) outcome; `MissingContentTypePolicy` lets each
endpoint decide whether a missing header is rejected or resolved to a declared default. `HarchWeb.Api`
does not implement CORS: it never reads `Origin`/`Access-Control-Request-Method` and never emits an
`Access-Control-*` header; an application that needs CORS preflight support composes its own handling in
front of its route family, keeping that policy application-owned rather than baked into every endpoint
declaration.
`selectRepresentation` negotiates a response media type from a server-preference-ordered list and an
optional `Accept` header per RFC 9110 §12.5.1 (bounded three-decimal quality values, wildcards,
specificity precedence, `q=0` exclusion, and `406` when nothing is acceptable). Endpoint handlers return a
typed `ApiResponse`; the declaration's non-empty `ApiResponseEncoder` list selects and encodes an
acceptable representation, sends its `Content-Type`, preserves application status/headers/observability
attributes/log entries, adds or merges `Vary: Accept` when alternatives exist, and returns 406 for an
explicitly incompatible `Accept`. A target's `ApiResponseBody` (the lower-level, non-negotiated response
shape) carries its own `apiResponseStatus`, defaulted to `200` by
`apiJsonResponse`/`apiTextResponse`/`apiBytesResponse` and overridden with a record update for an ordinary
typed non-200 outcome, e.g. `422` for input that decodes but fails semantic validation.

`HarchWeb.Api.Multipart` adds a bounded, incremental RFC 7578 consumer: a pure boundary scanner
(`newMultipartScanner`/`feedMultipartChunk`/`finishMultipartScanner`) that never retains more of a part's
body than the boundary delimiter's length, `parseMultipartFieldDisposition` for a part's `name`/`filename`,
and `withMultipartBodyWith`/`withMultipartRequestBodyWith` to drive the scanner against a chunked body
(any `IO ByteString` source, or a WAI `Request` directly). `MultipartStorage` makes durable backends an
explicit application choice; the WAI helper uses the supplied `InMemoryUpload` adapter, retaining no file
bytes beyond `MultipartLimits`' max-file budget. A caller-supplied callback runs as soon as each part
finishes, before any later part (including a later file part) is read, so a caller can reject the whole body
— on an invalid CSRF field, say — before a later file reaches storage. File data is available only via an
opaque scoped upload: promote it deliberately to take ownership, or leave it for automatic cleanup after
success, rejection, or an exception. `ApiMultipartRequestBody` is the typed endpoint's streaming multipart
request capability: it gives the handler a one-shot scoped consumer (`withApiMultipartRequest`) backed by
an application-selected storage adapter and `MultipartLimits`, preserving both duplicate-consumption
rejection and the multipart parser's typed failure. Decision (2026-08-18): filenames crossing either
the callback or adapter boundary are opaque `UntrustedFilename` metadata with only an explicit text
escape hatch; byte and item budgets use non-negative types, rather than eight bare `Int`s, so neither
kind of configuration error can silently remove a resource bound.

The disposition parser accepts only a case-insensitive `form-data` type and rejects duplicate `name` or
`filename` parameters. This keeps a part's metadata unambiguous before the consumer assigns resource
ownership; a missing `name` remains a typed consumer rejection (`MultipartMissingDisposition`).

`apiRouteDefinition` places one endpoint directly in an application's `RouteDefinition` table; for a
heterogeneous endpoint table, `SomeApiRouteEndpoint` wraps declarations of different types and
`apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` adapt the whole table into one
`HarchWeb.RouteCodec`/`RouteDefinition` route family (see the closed route-family registry below) — the
shared server dispatcher supplies the real WAI request only after it has selected the route and enforced
the table's method policy, so API declarations cannot create competing 404/405/`Allow`/`HEAD`/`OPTIONS`
behaviour with any other route family in the same application. A bounded buffered decoder maps oversized,
unsupported-media, and malformed input to 413, 415, and 400 respectively, and passes expected domain
failures through the endpoint's explicit interpreter. Additional streaming codec shapes and multipart
storage policy remain AC/AD follow-up work.

**Historical note (2026-08-13):** this typed endpoint boundary previously coexisted with two now-removed
compatibility layers: a legacy `ApiEndpoint`/`apiEndpointMiddleware` target-plus-handler table, and an
intermediate `apiRouteEndpointMiddleware` that derived `404`/`405`/`Allow`/`HEAD`/`OPTIONS` from a typed
`SomeApiRouteEndpoint` table composed as a standalone `Wai.Middleware` rather than through the route-family
registry. Both were deleted once every application in this repository had migrated onto the family
registry; see the AK decision record below for why that made deletion, not relocation, the right call.

### Decision record — AC typed declarative endpoint boundary (2026-08-12)

**Decision: extend the shared `RouteCodec`/`RouteDefinition` and server-response boundary (option 1),
not a second WAI dispatcher.** The existing route codec owns application path dispatch, while its
current shape lacked method-aware registration and a closed route-family registry. Method ownership is
now declared on each `RouteDefinition`; `buildSiteApplication` installs those declarations into the
shared `RouteCodec`, and the server derives 404, 405/`Allow`, `HEAD`, and `OPTIONS` there. That is the
first delivered portion of the decision: an application wrapper can no longer change a site's ordinary
method table. `ProtocolResponseResult` supplies the response half of that boundary: strict bytes or a
request-scoped WAI stream retain normal response security, diagnostics, and observability. The next
delivered portion adds `apiRouteEndpoint`/`apiRouteDefinition`: typed API fields and one bounded buffered
body consumer now run inside that same route table and response interpreter. Typed response values now use
declarative JSON, text, byte, or custom encoders with RFC 9110 `Accept` selection and `Vary: Accept`.
`RequestCodec` now also decodes case-sensitive cookie fields; malformed cookie fragments are ignored and
repeated names produce the same typed duplicate-field error as query and header fields. A declared bounded
`urlEncodedFormBodyDecoder` admits one `application/x-www-form-urlencoded` body against its field-count
cap before percent validation or query decoding, so an over-limit form cannot allocate an unbounded query
field list; it then decodes the admitted form;
`ApiUrlEncodedFormRequestBody` makes that form decoder an endpoint's one body consumer; it adds its fields
to query/header/cookie request data before the same accumulating `RequestCodec` runs through `formField`.
This preserves parse failure separately from field validation. Response `Accept` media parameters now match
declared `Content-Type` parameters (including a quoted, case-insensitive UTF-8 charset); ranges after `q`
are extensions and do not constrain matching. Typed response encoders may now emit strict bytes or a
request-scoped WAI stream, preserving the existing server response boundary without materializing a lazy
body. `ApiStreamingRequestBody` declares the request-side counterpart: the delivered
`ApiStreamingRequest` gives the handler one bounded chunk at a time via
`HarchWeb.Server.RequestBody.newRequestBodyChunkReader`, which enforces the same running-total
budget `readRequestBodyUpTo` enforces over a single buffered read, without the framework retaining
the body itself — so the framework's own memory use stays bounded by the current chunk plus a byte
count, not by body size. Unlike a buffered body's automatic 413, a pull that would exceed the
budget reports `RequestBodyReadFailure` to the endpoint's own handler rather than the framework
short-circuiting the response, since a true stream's limit violation can only be detected after the
handler has already begun consuming earlier chunks — the framework cannot retroactively take over
mid-handler the way it can reject before ever calling a buffered-body handler. This mirrors how a
multipart parser failure already stays typed for the handler instead of becoming an automatic
response. See below for the closed route-family registry. `ApiMultipartRequestBody` now lets an API
route select the existing scoped multipart consumer exactly once; its storage adapter and staged
ownership remain AD policy, so the endpoint does not create a new upload lifecycle or default to
local files.

`runServerWithWaiMiddleware`/`withLocalTestServerForApplication` are the raw-WAI composition points
for a declared route-family application against a real running (or locally test-served) server, not
just a bare `Wai.Application` in a unit test. Typed applications instead use `runServer` and
`withLocalTestServer`, which apply their declared request policy as the `id`-middleware case. [multipart-upload](../examples/multipart-upload/README.md)'s `/native-upload`
page (`App.MultipartUpload`) is the compiled, tested demonstration of the whole native-upload slice —
migrated 2026-08-13 onto `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition`, the second
application (after `examples/custom-api`) off the legacy `apiEndpointMiddleware`, so its own
`RouteDefinition` now owns 404/405/HEAD/OPTIONS for the whole example rather than falling through to a
hand-written `Wai.Application`: a plain `<form enctype="multipart/form-data">` with no `data-harch-action`
attribute (so the inline capture kernel's `form[data-harch-action="true"]` selector never matches it and
the browser submits it natively, with or without JavaScript), a single-use server-held CSRF token
embedded as a hidden field (chosen over a double-submit cookie because nothing in this framework version
lets a plain page response set a `Set-Cookie` header), and `withApiMultipartRequest` — the typed
endpoint's scoped consumer over the same declared `ApiMultipartRequestBody` — validating that field via
its per-part callback — before any later part, including the file part, is read — so a request whose file
part follows an invalid or absent CSRF field is rejected before that file reaches storage. See
the module haddock in `examples/multipart-upload/src/App/MultipartUpload.hs` for the full policy, and
`examples/multipart-upload/test/E2E/MultipartUploadSpec.hs`'s two native-upload scenarios (scripts enabled and disabled) for the real-browser
proof that the submission is a hard navigation with zero capture-kernel mutation requests and exactly
one deliberate in-memory upload discard either way.

#### Follow-up decision — closed route-family registry (2026-08-12)

**Decision: extend `HarchWeb.Routing.RouteCodec` with a generic two-family combinator (option 1
again), rather than a third parallel dispatcher.** `apiRouteEndpointMiddleware` (above) still only
owns the paths it matches and shares no 404/405/`Allow`/HEAD/OPTIONS authority with whatever
`Wai.Application` it wraps — composing it around a `buildSiteApplication` output recreates the exact
two-independent-dispatchers shape the "Extend an existing boundary" worked example in this
document's decision framework warns against, just with `HarchWeb.Api`'s own middleware playing that
role instead of an application-authored one. `HarchWeb.Routing.RouteFamily`/`combineRouteCodecs`
closes that gap at the primitive level: `combineRouteCodecs` merges two `RouteCodec`s into one,
trying the first family before the second (so an overlapping path deterministically belongs to the
first, the same declaration-order precedence `matchRouteMethod` already applies within one family),
and using the second/catch-all family's `notFoundRequest` as the combined not-found — nest it to
combine more than two. `HarchWeb.Api.Endpoint.apiRouteEndpointFamilyCodec` adapts a
`SomeApiRouteEndpoint` table into that shared `RouteCodec` family (route identity = the matched
endpoint's declared `ApiPath`; `routeMethods` reports every method declared at that path so `HEAD`/
`OPTIONS`/405 keep deriving from the one shared `HarchWeb.Routing` implementation, not a
second copy), and `apiRouteEndpointFamilyDefinition` supplies the matching `RouteDefinition`. When
its path has a declared endpoint, it selected the one whose declared method matched the real request
via the then-partial `matchedApiRouteEndpointOrDie` — reachable only by a framework wiring defect
there (an inconsistent endpoint table between the codec and the definition), since every request method
`routeResponse` sees for a *declared* path has already been validated against this same family's
`routeMethods` by the shared dispatcher before `routeResponse` runs.

This closes the primitive gap named in "A closed route-family registry ... remain AC steps" above.

### Follow-up decision — standalone family not-found and the custom-api migration (2026-08-13)

**Fix, not a new decision: `apiRouteEndpointFamilyDefinition` must not call its then-partial
endpoint matcher for a path with no declared endpoint.** Migrating
`examples/custom-api` onto `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` (below)
surfaced a real defect the primitive's own tests never exercised: `apiRouteEndpointFamilyCodec`'s
`notFoundRequest` resolves every unmatched path to the same synthetic `ApiPath ""` route, which has
no declared endpoint, but `apiRouteEndpointFamilyDefinition`'s `routeResponse` unconditionally
called its then-partial endpoint matcher for whatever path it was given — including that synthetic one
— throwing an uncaught `error` instead of rendering a `404`. Every landed test combined the family
with a catch-all second family via `combineRouteCodecs`, whose own not-found route absorbs the
not-found case before `apiRouteEndpointFamilyDefinition` ever sees the synthetic path, which is why
this went unnoticed until an application used the family **standalone** — precisely
`examples/custom-api`'s shape, since it has no page family to combine with. `web-api`'s route ADT
does not have this exposure even before any migration: its own `ApiNotFound`/`PageNotFound`
constructors are real, declared members of its route table with their own `RouteDefinition`, not a
codec-synthesized sentinel outside the table. Fixed by having `apiRouteEndpointFamilyDefinition`
check for a declared endpoint at its given path first and render an ordinary API `404` directly when
none exists, before ever reaching the matcher. The former matcher was subsequently deleted by BP; a
new Unit test
drives `apiRouteEndpointFamilyDefinition`'s `routeResponse` on the codec's own `notFoundRequest`
route end to end (not just the pure `RouteCodec` value) to keep this path covered.

**`examples/custom-api`'s `App.Api.Declarative` is migrated.** It now declares its three endpoints as
`SomeApiRouteEndpoint` values, composed through `apiRouteEndpointFamilyCodec`/
`apiRouteEndpointFamilyDefinition` into an ordinary `HarchWeb.Site.Site` (via `simpleSite`), rather
than the legacy `apiEndpoint`/`apiEndpointMiddleware` pair. Since this example has no page routes, its
`Site` carries a `PageShell` that no declared route ever renders — an ordinary, if unusual, total
value, not a workaround. The migration also dropped the module's former hand-rolled
`selectRepresentation` call and its accompanying `Maybe ApiMediaType`-shaped defensive failure path
for an "invalid configured representation": that state cannot arise once each `ApiResponseEncoder`'s
content type is a fixed value declared once, rather than parsed from a `Text` literal at response
time, so `apiRouteEndpointFamilyDefinition`'s own negotiation replaces it outright. The migration also
surfaces one genuine, pre-existing behavior difference from the framework's typed request-data
extraction (not introduced by this migration): an invalid-UTF-8 `Accept`/`Content-Type` header is
leniently decoded (replacement characters), not treated as absent, matching how
`apiRequestDataFromWaiRequest` already treats every other header/query value — so a garbled `Accept`
now yields `406` rather than silently falling back to the default representation the way the
example's old hand-written `Maybe`-based lookup did.

`web-api`'s own `AppRoute = Page PageRoute | Api ApiRoute` still hand-writes its own combined path
parsing/rendering instead of using `combineRouteCodecs`, and its `/api/status`/`/api/second` routes
still dispatch through ad hoc `WebApi.Route`/`WebApi.Response` logic that does not call into
`HarchWeb.Api.Endpoint` at all. This remains unstarted, and is now a materially larger lift than the
custom-api migration was: `/api/second`'s database-failure path attaches specific observability
attributes and a private log entry to its response (`WebApi.Response.renderApiResponseFromRouteDataWithOperations`),
and the typed endpoint boundary's `ProtocolResponse` construction in
`HarchWeb.Api.Endpoint.renderEndpointResult` currently hardcodes
`protocolResponseObservabilityAttributes = []`/`protocolResponseLogEntries = []` with no way for a
handler to attach either — a capability gap the typed boundary would need before this specific
migration could preserve `web-api`'s existing DB-failure observability, not merely a bigger version of
the same mechanical rewrite. Migrating `web-api` remains tracked as part of AC in `TASKS.md`; until
then, its own hand-rolled dispatch is the only path it actually uses.

### Follow-up decision — typed endpoint observability attributes and log entries (2026-08-13)

**Decision: add the primitive to `ApiResponse` (option 1), the small-general-framework-owned choice
from "When implementation hits a missing framework capability."** The gap identified just above —
`HarchWeb.Api.Endpoint.renderEndpointResult` hardcoded `protocolResponseObservabilityAttributes = []`
and `protocolResponseLogEntries = []`, so a typed endpoint handler had no way to attach either — is
small, squarely within `HarchWeb.Api.Response`'s existing ownership of what an `ApiResponse` carries,
and general: every typed endpoint that wants request-scoped diagnostics (a database failure code, a
downstream timeout) needs the same capability `HarchWeb.Server.Response.ResponseBody` already gives
page routes. `ApiResponse` gained `apiEndpointResponseObservabilityAttributes ::
[Observability.ObservabilityAttribute]` and `apiEndpointResponseLogEntries :: [Text]`, defaulted to
`[]` by the `apiResponse` smart constructor and overridden with a record update exactly like
`apiEndpointResponseHeaders` already is; `renderEndpointResult` now forwards both onto the rendered
`ProtocolResponse` instead of discarding them. Both fields are private diagnostics carried alongside
the public response, never inside its encoded body — the same separation `ResponseBody` already
enforces for pages, so a handler cannot accidentally leak them to a client by construction (they have
no encoder). This closes the specific capability gap named above; migrating `web-api`'s
`/api/second` onto the typed endpoint boundary remains separate, larger follow-up work (it also needs
the family-registry migration itself, not just this capability).

### Follow-up decision — multipart-upload migration and a genuinely dead legacy surface (2026-08-13)

**`examples/multipart-upload`'s `App.MultipartUpload`/`App.App` are migrated onto the same
`apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` pair as custom-api**, following the
custom-api decision record above rather than a new one: `nativeUploadEndpoints` became a
`NativeUploadState -> [SomeApiRouteEndpoint]` function (state is captured per-endpoint-declaration
instead of threaded separately into a shared handler), its two declarations use
`ApiNoRequestBody`/`ApiMultipartRequestBody inMemoryMultipartStorage defaultMultipartLimits`, and its
CSRF-gated per-part multipart callback is unchanged except for calling the endpoint's own
`withApiMultipartRequest` scoped consumer instead of `withMultipartRequestBodyWithStorage` directly —
the same underlying scanner and storage adapter either way. `App.App`'s `multipartUploadApplication`
builds the typed `Application` via `apiOnlySite` + `buildSiteApplication`; that site boundary keeps one
shared route dispatcher, disables page-navigation runtime, and retains a minimal complete-SSR fallback
if a future route renders a page. Its executable-only WAI adapter is private. The Unit suite renders it
through `toWaiApplication`, while the real-browser E2E
suite passes the typed value to `withLocalTestServer`, so that test listener applies the site's same
request-head, transport, and concurrency policies. This avoids pretending an opaque WAI function carries
policy metadata or adding a second raw-WAI policy composition API. It replaced
`apiEndpointMiddleware` wrapping a hand-written `notFoundApplication`. All 14 Unit tests and
both real-browser E2E scenarios passed unchanged except one: a request to an undeclared path now
receives the framework's standard response security headers (CSP, `X-Content-Type-Options`, etc.) on
its `404`, because it now renders through the same `Site`/`toWaiApplication` pipeline as every other
response instead of a bespoke fallback `Wai.Application` that never applied them — a genuine
correctness improvement the migration exposed, not a regression to work around.

This closes the prerequisite the AK entry in `TASKS.md` named: with both example applications off
`apiEndpointMiddleware`, and neither ever having used the intermediate `apiRouteEndpointMiddleware`
either, `HarchWeb.Api.Endpoint`'s legacy `ApiEndpoint`/`apiEndpointMiddleware` surface and its
`apiRouteEndpointMiddleware` typed-WAI-middleware composition function are both now unreferenced by any
application in the repo — only `HarchWeb.Api`'s own facade re-export and `HarchWeb.Api.Endpoint`'s own
`ApiSpec.hs` tests still call them. Per this document's "Treat this framework as a versioned foundation"
posture, that makes deleting both a legitimate breaking change rather than something requiring a
migration path — but actually doing so (auditing exactly what can be deleted versus what
the then-public endpoint matcher still shares, updating both export lists, deleting the now-pointless
dedicated tests, and updating every doc that still describes the compatibility helpers as current) is
real follow-up work of its own, tracked under AK, not completed as part of this migration.

### Follow-up decision — AK: deleted the legacy compatibility surface (2026-08-13)

**Decision: delete, not relocate — completing AK's investigated deletion candidate.** With both
example applications migrated, a repository-wide audit confirmed no application code anywhere still
called `ApiEndpoint`/`apiEndpoint`/`apiEndpointTarget`/`ApiMatchResult`/`matchApiEndpoints`/
`matchLegacyMethod`/`legacyEndpointAtPath`/`legacyEndpointHasMethod`/`legacyDeclaredMethods`/
`respondApiMatch`/`legacyRenderedApiResponse`/`apiEndpointMiddleware` (the legacy target-plus-handler
table) or `apiRouteEndpointMiddleware`/`matchApiRouteEndpoints`/`protocolResponseToWaiResponse`/
`requestPathTextFromWai` (the intermediate typed-WAI-middleware composition) — only `HarchWeb.Api`'s own
facade re-export and `HarchWeb.Api.Endpoint`'s own dedicated `ApiSpec.hs` tests referenced them. Per
this document's "Treat this framework as a versioned foundation" posture and the missing-capability
protocol's option 1 (small, general, squarely within an area the framework already owns), this was a
genuine deletion, not a `.Internal`-module relocation: all of the above were removed from
`HarchWeb.Api.Endpoint` and `HarchWeb.Api`'s export list, along with their now-pointless dedicated tests
(the "legacy API endpoint compatibility", "apiRouteEndpointMiddleware", "apiHttpResponseToWaiResponse",
and "apiEndpointMiddleware" `describe` blocks in `ApiSpec.hs`, and the standalone `apiAllowHeaderValue`
test, since that function had no other caller once both middlewares were gone). `apiHttpResponseToWaiResponse`
and `apiAllowHeaderValue` were deleted too, once auditing confirmed their only remaining callers were the
two removed middlewares. `ApiHttpResponse` and `apiHttpResponseToProtocolResponse` were kept: unlike the
deleted functions, `apiRouteEndpointFamilyDefinition`'s own not-found rendering (the AC standalone-not-found
fix above) depends on both.

**Follow-up (same day): the deletion's own coverage run surfaced dead code the deletion itself created,
not a testing gap.** Running the full CI-equivalent coverage gate against the deletion (rather than only
`cabal build`/Unit tests, which had been the extent of verification up to that point) failed at 99%, not
100%, isolated entirely to `HarchWeb.Api.Endpoint`. Per this document's ban on forcing a coverage tick with
`$!`/`seq`/fake work, each gap was root-caused rather than papered over:

- `ApiRouteMatch`'s `TypedApiRouteMethodNotAllowed`/`TypedApiRouteOptions` branches inside
  `matchApiRouteMethod` were **genuinely dead**, not merely untested: the then-partial endpoint matcher was
  their only consumer, and it already treats every non-`Matched`/`MatchedHead` outcome identically (an
  `error` naming the wiring defect) — so distinguishing "method not allowed" from "OPTIONS" was complexity
  with no surviving behavioral difference, left over from when the deleted legacy dispatcher rendered `405`
  and `204`/`Allow` differently for the same `ApiRouteMatch` value. Fix: deleted `ApiRouteMatch` and
  `matchApiRouteMethod` outright and inlined the one remaining match/HEAD-fallback/die decision directly
  into that matcher — restructuring the code per this document's guidance, not adding a
  test for a branch nothing can ever reach.
- The remaining gap, after that restructuring, was three lazily-unforced sub-expressions, not a tooling
  artifact: HPC's per-expression box for a `case` branch or function body ticks when that code path is
  *entered*, but a lazy field inside it can still show as never executed if nothing downstream actually
  demands its value. Comparing the coverage HTML's own tick markup (`<span class="istickedoff">…<span
  class="nottickedoff">…</span>…</span>`) showed each surrounding branch genuinely ran, while only an inner
  atom stayed dark: the not-found response's `[]` headers and `Nothing` body (the only not-found test asserted
  the status, never forcing the other two `ProtocolResponse` fields), `TextEncodingError.lenientDecode`
  passed to `decodeUtf8With` inside `requestMethodTextFromWai` (never forced because no test ever sent a
  non-UTF-8 request method through the typed family dispatcher, so `decodeUtf8With`'s error-recovery callback
  was never invoked), and `methodNotDeclared` inside the former endpoint matcher's `HEAD`-with-no-`GET`
  fallback (never forced because no test sent `HEAD` to a path whose only declared endpoint was `POST`).
  All three were real behavior the deleted "apiRouteEndpointMiddleware" test block had covered from the
  other side (a WAI-level 404/405 assertion) before this migration's predecessor commit removed it — genuine
  coverage losses, not tool quirks. Fix: three real tests, not forced ticks — extended the not-found test to
  assert empty headers/body, and added dedicated tests for a malformed non-UTF-8 method and a `HEAD` request
  against a `POST`-only table, each previously asserting the former matcher's defensive `error`.
- The same deleted "legacy API endpoint compatibility" test block had been the only place exercising
  `ApiMethod` and `ApiPath`'s derived `Eq`/`Show` (including `showList`, the method a derived `Show`
  instance also generates but which nothing else in the codebase calls) directly, comparing every pair of
  sample values and calling `show`/`showList` on each; `ApiHttpResponse`'s derived `Eq` lost its only
  exerciser the same way, from the deleted "renders every legacy match outcome" test. Fix: added "retains
  comparable, printable endpoint values" (`ApiSpec.hs`), scoped to the three types that still exist,
  verifying derived `Eq` agrees with itself and is irreflexive under `/=`, and that `show`/`showList` both
  produce non-empty output — a legitimate assertion about derived-instance behavior, not one written only to
  satisfy the coverage percentage.

`HarchWeb.Api.Endpoint` shrank from 658 to 499 lines (41 declarations, 22 imports, 23 exports, fan-out 9,
per `tools/haskell-quality-report.sh`) by the combined legacy-surface deletion and this coverage-driven
dead-code removal. **This closes the AK module-health signal**: this document's module-health rule is a
conjunction — a module must exceed 500 lines *and* (20 imports or 10 local dependencies/fan-out) to be
flagged — and at 499 lines the module no longer exceeds the line threshold regardless of its 22 imports or
fan-out of 9, both otherwise-unremarkable numbers for a module gating an entire typed API surface. Naming
the margin honestly: 499 is one line under the threshold, not a wide margin, so a future addition to this
module should re-run `tools/haskell-quality-report.sh` rather than assume the signal stays closed.
Re-examining a `.Family` module split now that only one dispatch path remains (the earlier "shared by
legacy and typed-middleware" obstacle is gone): the blocker AK's original investigation named independently
of that sharing — `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition`/
the former endpoint matcher and its supporting matchers (`endpointAtPath`, `endpointHasMethod`,
`declaredMethods`) all need to pattern-match the `ApiPath` newtype's constructor, which
`HarchWeb.Api.Endpoint`'s export list deliberately keeps private (`ApiPath` the type, not `ApiPath(..)`) —
still applies verbatim, but is no longer required work now that the module-health metric itself is
satisfied: a `.Family` split remains available as a future readability improvement, not a follow-up this
document tracks as owed.

### Follow-up decision — endpoint module-health split (2026-08-18)

**Decision: split the existing endpoint boundary internally, not by adding an endpoint abstraction or
exposing `ApiPath`.** The subsequent total-handler addition grew `HarchWeb.Api.Endpoint` to 704 lines and
22 imports, re-opening the repository's non-facade module-health signal. The boundary and public API remain
correct: the missing capability was ownership separation, not another routing surface. The public
`HarchWeb.Api.Endpoint` module is therefore now a facade over three private collaborators:
`Endpoint.Internal` owns the representations and the deliberately non-exported `ApiPath` constructor,
`Endpoint.Family` owns the one `RouteCodec`/`RouteDefinition` family adapter and its defensive invariant
check, and `Endpoint.Runtime` owns request-body decoding and protocol-response interpretation. This follows
the extend-existing-boundary rule: callers still use exactly one route-family dispatcher, while the private
family interpreter can inspect the abstract public path without widening the public API. The split is not a
relocation of a compatibility surface and does not create a second WAI dispatcher. Focused endpoint tests,
the full 100% coverage gate, and the module-health report are the required proof of this structural change.

### Follow-up decision — AC's web-api gap is a single-dispatcher extension, not a family (2026-08-13)

**Decision: extend `web-api`'s existing single `AppRoute`/`routeCodec` dispatcher via
`HarchWeb.Api.Endpoint`'s per-route `apiRouteDefinition`, not `combineRouteCodecs`/`RouteFamily`.**
Investigated before writing any code, per this document's own "extend an existing boundary before
adding a parallel one" rule: `examples/custom-api` and `examples/multipart-upload` both had two
genuinely separate codecs (an API family plus a catch-all/page family) that needed one combined
not-found/dispatch authority, which is exactly what `combineRouteCodecs` is for. `web-api`'s
`AppRoute = Page PageRoute | Api ApiRoute` (`WebApi/Route.hs`) is already one closed sum type behind
one `routeCodec`/`Site.simpleSite` call — pages and APIs were never two competing codecs there, so
wrapping it in a family combinator would rename an existing guarantee rather than add one, at the
cost of rippling through `AppRoute`'s pervasive use in navigation, `PageLink`, `RouteMetadata`, and
roughly 600 `WebApiSpec.hs` assertions. `apiRouteDefinition` (`HarchWeb.Api.Endpoint`, distinct from
the family pair) is deliberately built for this shape instead: its Haddock already documents "the
server has already selected the route and method before this runs," i.e. compose one typed endpoint
into an *existing* dispatch table's per-route slot.

That investigation also surfaced a real, narrow missing-capability gap: `apiRouteDefinition`'s
`routeResponse = \request _ -> ...` discards the `RouteRequest route context` argument
`RouteDefinition`'s own signature already provides, and `ApiRouteEndpoint`'s handler
(`ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))`) has no
parameter to receive it even if it weren't discarded — so an endpoint composed this way cannot see
anything the route's own codec already parsed (here, `web-api`'s locale, derived from a URL prefix,
not from any query/header/cookie field `HarchWeb.Api.Request`'s `RequestCodec` can decode). Per the
missing-capability protocol this is option 1 (small, general, squarely within
`HarchWeb.Api.Endpoint`'s own area).

**Implemented (2026-08-13): `apiRouteDefinitionWithContext` and its `web-api` consumers landed
together.** `/api/status` and `/api/second` now dispatch through `HarchWeb.Api.Endpoint`'s typed
endpoint boundary instead of `WebApi.Response`'s hand-rolled `ApiRoute` dispatch; `WebApi.App`
special-cases only these two routes in `buildAppRouteDefinition`, leaving the old dispatch in place
(unmodified in behavior, only newly exporting a few pure helpers for reuse) since it remains the
genuine path for `Api ApiNotFound`. This shipped as the complete slice the earlier entry described —
not a narrower one — but it took three implementation passes to reach a genuinely 100%-covered,
non-fake-strictness state, each catching a different way "looks precise" code traps this repository's
coverage gate rather than merely being undertested:

1. **First design:** `apiRouteDefinitionWithContext` built a full `ApiRouteEndpoint` from a
   "template" value with a placeholder handler field, overridden via record update before running it.
   Since `ApiRouteEndpoint`'s fields are lazy, the placeholder was never forced — genuinely dead code,
   not a testing gap. **Fix:** redesigned to call the endpoint's runtime dispatch logic
   (`runApiRouteEndpointHandler`, itself extracted from `runApiRouteEndpoint`) directly with exactly
   the pieces it needs, never constructing a placeholder-bearing value at all.

2. **`Data.Void`/`absurd` for a handler that cannot fail:** `/api/status`'s handler always succeeds,
   so its first cut modeled that with `Void`/`absurd` — the standard "make invalid states
   unrepresentable" idiom. This type-checks and looks precise, but traps the coverage gate: `either`
   never evaluates its first argument on a `Right`, so a handler that only ever returns `Right` never
   forces the failure-response value, and when that value's type is `Void`, no test can force it any
   other way either — no `Left` of type `Void` can ever be constructed. This is not a testing gap; the
   expression is permanently unreachable by construction, in any module, no matter how the code is
   phrased. **Fix:** added a second, narrower framework primitive,
   `apiRouteDefinitionWithContextNeverFailing`, whose handler type has no `Either`/failure-response
   parameter at all — so there is no unreachable expression to leave dead. Its runtime logic
   (`runApiRouteEndpointHandlerNeverFailing`) shares the request-decoding half
   (`runDecodedApiRequest`, extracted once, used by both) with the `Either`-based path, differing only
   in how a decoded request becomes a final response.

3. **CAF-sharing across separate top-level definitions:** even after (1) and (2), the coverage gate
   still failed intermittently as different pieces landed, each time on a bare literal
   (`[HarchWeb.RouteGet]`, `pure ()` for a no-fields `RequestCodec`) written identically in two
   separate places (`WebApi.Route`'s `Page _`/`Api _` branches; `statusApiRouteDefinition` and
   `secondApiRouteDefinition`'s field codecs). GHC shares such literals into one CAF, and only one of
   the two call sites' HPC ticks ever fires even though both genuinely execute — the same pattern
   already documented for `HarchWeb.Action` literals. `$!`/`seq` forcing (the technique that closed
   that earlier case) does not generalize here: HLint correctly flags forcing an already-WHNF `()` or
   list literal as redundant, so using it would trade one gate failure for another. **Fix:** removed
   the duplication instead of forcing around it — factored the shared `pure ()` into one named,
   exported `noApiRequestFields` binding used by both endpoints. One genuinely remaining case (the
   *decoded* `()` value itself, produced by running that codec) was never forced by any test because
   neither handler reads its decoded fields and `case ... of Just decodedFields -> ...` only forces
   the `Just`, not its contents — closed with a direct Unit test that calls `runRequestCodec
   noApiRequestFields ...` and asserts on the result, forcing it through `Eq`, matching this
   codebase's other direct-`RequestCodec`-exercise tests rather than routing through the full endpoint
   pipeline where nothing reads the value.

Investigating the *last* apparent gap (`WebApi.Route`'s `Api _ -> [HarchWeb.RouteGet]`) surfaced a
fourth, different lesson worth its own note: it looked like more CAF-sharing but wasn't.
`HarchWeb.buildSiteApplication` overrides a `Site`'s route codec so its `routeMethods` derives from
each route's *live* `RouteDefinition` (`routeMethods . siteRouteDefinition site`) rather than the
codec's own declaration — and `buildAppRouteDefinition` now special-cases `StatusApiRoute`/
`SecondApiRoute` before ever reaching the branch that would consult `WebApi.Route`'s own
`routeMethods`. A test asserting on `HarchWeb.routeMethods (HarchWeb.routeCodec pureApplication) ...`
for those two routes was therefore exercising a *different, coincidentally same-valued* code path,
not `WebApi.Route`'s own declaration — no amount of restructuring the declaration itself could have
fixed that. The fix was testing `WebApi.Route.routeCodec` directly, bypassing the application-level
override, so the assertion exercises the declaration it actually names.

See the AC entry in `TASKS.md` for the full investigation and final numbers.

### Follow-up decision — AF: concurrent-in-flight-request admission gate (2026-08-13)

**Decision: an opt-in, non-blocking WAI-level admission gate shared by every real listener,
including the local test harness — not a Warp setting, since none exists.** Investigated first:
Warp 3.4.12 (the pinned version) has no concurrent-request or connection-count setting of its own,
matching the same kind of documented limitation this document's AF header-count precedent already
established for Warp's header handling — so this could not be "add the missing Warp option" and had
to be a framework-level admission gate instead. Built as `HarchWeb.Server.RequestExecution.concurrencyLimitedMiddleware ::
Maybe RequestConcurrencyLimit -> Wai.Middleware -> IO Wai.Middleware`, composed around the same
`Wai.Middleware` seam `runServerWithWaiMiddleware` already exposes, so it sits in front of an
application's own middleware rather than displacing it. Admission is immediate accept-or-reject (a
non-blocking counting `IORef`, not a `QSem` queue): a caller beyond the limit gets a stable `503`
before route parsing, middleware, observability, or body reads, matching the existing request-head
gate's contract rather than making the caller wait for a slot — a queueing gate would change the
task's own resource-protection intent into a hidden latency amplifier under load. A held slot is
released via `finally`, covering the request's whole lifetime including a streamed response, on
ordinary completion or any exception.

A real mid-implementation catch worth recording: the gate initially lived only in
`HarchWeb.Server.Runtime`, and its first real-socket test (via `HarchWeb.Server.LocalTest.withLocalTestServer`)
deadlocked, because `withLocalTestServer` builds its own local Warp listener directly from
`toWaiApplication` and never went through `Runtime.hs` at all — the configured limit had zero effect,
so both a slow first request and a second concurrent one were admitted, and the second's handler
blocked on the same release signal the test itself was waiting to trigger. This is exactly the kind
of gap the "extend an existing boundary" worked example warns about in miniature: two composition
paths (`Runtime.hs`'s real listener, `LocalTest.hs`'s test listener) had each grown their own way of
applying `RequestPolicyConfig`, and a new field only reached one of them by construction. Fixed by
moving the gate into the shared `RequestExecution` module both already import, so a real-socket test
against the local harness now observes the identical admission behaviour a deployed runtime would.

A second coverage catch, smaller but worth naming precisely since it cost real time to isolate: a
plain `deriving (Eq, Show)` on `RequestConcurrencyLimit` (a `newtype` wrapping `Int`, structurally
identical to sibling types like `RequestByteLimit` that derive `Eq` without issue) left its `Eq`
instance's own "declaration used" box unticked under the coverage gate even when tests called `==`/`/=`
on it directly and other tests exercised its `Show`. `HarchWeb.Security` already has an established,
working answer to exactly this newtype-`Eq`-under-HPC gap: `RequestItemCountLimit` derives only `Show`
and hand-writes `instance Eq RequestItemCountLimit where RequestItemCountLimit left == RequestItemCountLimit right = left == right`.
Matching that precedent (rather than re-investigating why derived `Eq` sometimes doesn't tick) resolved
it once the test also called `==`/`/=` as a direct boolean expression rather than routing the
comparison through `shouldBe`/`shouldNotBe`'s polymorphic `Eq a =>` dictionary — restructuring the
code to match a working precedent, not forcing a tick.

### Follow-up decision — AL: split `HarchWeb.Security.RequestLimits` out, not the rest (2026-08-13)

**Decision: extract only the genuinely self-contained cluster; leave the coupled remainder unsplit.**
`HarchWeb.Security` (763 lines, 48 exports) was over this document's 40-export module-health
threshold, worsened marginally by AF's new `RequestConcurrencyLimit` exports. Unlike AK's earlier
blocked "naive split" attempt (two dispatch paths shared private matching primitives with no clean
boundary), this module's exports fall into four natural concern clusters: request limits, response
security headers/CORS/CSP, request-context/observability, and path/redirect. Investigated with real
call-graph tracing (not export-name grouping) before writing any code: the request-limits cluster
(`RequestByteLimit`, `RequestConcurrencyLimit`, `RequestHeadLimits`, `RequestHeaderCountLimit`,
`RequestItemCountLimit`, `RequestTimeoutSeconds`, `RequestTransportLimits`,
`RequestHeadLimitFailure`, `validateRequestHead`, and their private helpers) never references
`RequestPolicyConfig` or anything in the other three clusters — a genuinely closed subgraph. The
other three clusters are not: response-header construction calls `requestScheme` (request-context),
and path/redirect handling calls the private forwarded-header trust chain that also backs
request-context extraction, so splitting those three apart would mean either widening an
internal-only export surface or duplicating logic across the split — the same "relocation, not a
genuine shrink" trap AK's investigation already named. Per this document's "extend an existing
boundary" and missing-capability discipline, the correct move was extracting only the closed
subgraph, not forcing a three-way split to hit a metric.

Extracted `HarchWeb.Security.RequestLimits` as an `other-modules` entry (not independently
importable — matching `HarchWeb.Api.Endpoint`'s precedent of an internal module re-exported by its
public facade). `HarchWeb.Security` re-exports it wholesale via `module HarchWeb.Security.RequestLimits`
in its own export list, so no existing consumer's import list changed at all — this is purely an
internal reorganization, not a public API change, and needed no `CHANGELOG.md` entry for that reason.
`HarchWeb.Security` is now 556 lines / 30 exports; `HarchWeb.Security.RequestLimits` is 243 lines / 19
exports. Full CI-equivalent pipeline passed with zero test changes, since the moved code's behavior
and existing coverage carried over unchanged.

### Follow-up decision — BP: total action-target and API-family matching (2026-08-18)

**Decision: extend the existing `ActionCodec` and route-family interpreter (option 1), rather
than adding a second action registry or treating configuration defects as server exceptions.** An
action's path, method, decoder, form rendering, and server dispatch already belong to
`ActionCodec`; a separate proof/registry would duplicate the declaration table and reintroduce a
drift point. `actionPath` and `actionMethod` therefore report an absent target as `Nothing`, while
`Controls.actionForm` returns an explicit `ActionFormRendering` value. Rendering that result
preserves the authored child content and emits an accessible configuration diagnostic, but never
emits the capture markers that would claim an undeclared action is ready. This makes the required
application choice visible in the type at every SSR component boundary instead of silently
dropping the control. `ActionDecoder` retains its composable applicative representation and
normalizes its only malformed third-party convention, `([], Nothing)`, to the stable
`InvalidClientActionDecoder` result rather than throwing, while retaining accumulated parse errors.

The API route-family's matcher is now private and total as well: after the family definition has
proved a path has a non-empty endpoint set, matching its real request method returns `Maybe`; the
only defensive mismatch response is a typed `405` with the same declaration-derived `Allow` value.
The shared route dispatcher remains the normal owner of 404/405/HEAD/OPTIONS policy, so this is a
totality guard for direct or inconsistent invocation, not a parallel dispatcher.

### Follow-up decision — DV: typed HTTP response statuses (2026-08-17)

**Decision: extend the existing response boundary with `http-types`' `Http.Status`; do not add a
framework status wrapper.** `ResponseBody` and `ClientActionResponse` already are the single
framework-owned point where application responses become WAI responses. A bare `Int` there admitted
invalid statuses and required every rendering path to discard the standard reason phrase with
`mkStatus status mempty`. `Http.Status` is the established WAI representation and provides both the
validated standard constants applications need and the reason phrase WAI must receive. A new
newtype would duplicate it without improving composition or preventing an additional invalid state.
Observability remains explicitly numeric: `responseStatusCode` extracts `Http.statusCode` only when
constructing the low-cardinality HTTP server attribute.

### Follow-up decision — CJ: published dependency bounds (2026-08-17)

**Decision: put PVP `^>=` bounds on every direct dependency in each project-owned package
manifest; do not substitute a repository-only freeze file.** Dependency compatibility is part of a
published package's contract, while `cabal.project` constrains only this checkout and would leave a
Hackage consumer of an older release exposed to later incompatible APIs. The lower bounds are the
versions in the current successful build plan; PVP supplies the corresponding compatibility ceiling.
This applies uniformly to library, executable, test, custom-setup, and build-tool dependencies, so
the package can be configured reproducibly as a whole. The vendored
`hspec-expectations-match` manifest retains its upstream bounds rather than being rewritten as a
project API decision. Future dependency upgrades must deliberately update the relevant lower bound
and pass the complete release gate, rather than silently widening an old release's solver range.

### Follow-up decision — CM: remove Custom setup hooks (2026-08-18)

**Decision: use Cabal's existing build-tool dependency graph for `core`, `harch-web`, and
`test-core`, and use `Hooks` only for `web-api`'s genuinely separate database lifecycle.** The
first three Custom setups had become identical manual calls to Cabal's build hook before testing.
Their `build-tool-depends` declarations already express the real prerequisite relationship, so
keeping a parallel setup-time build path duplicates ownership and hides that dependency from Cabal.
They now use `build-type: Simple`, with no `Setup.hs`; the obsolete dummy `test-core` executable and
the `Core.Setup` forwarding module are removed too. The `test-core` source used by `core` is a
checked-in snapshot, verified by the package-manifest gate, rather than an old setup-time copy.

`web-api` is different: its optional local database autostart is a package lifecycle operation, not
a component dependency. Cabal Hooks has no Custom-style test hook, so the migration deliberately
does **not** recreate one. Its pre-configure hook records whether the existing prerequisite checker
actually started a database; the post-build hook runs migrations only after Cabal builds the already
declared `haskell-web-api-db` executable. This retains the opt-in workflow without claiming a
second build order. The manifest gate asserts the three Simple packages and the checked-in Hooks
implementation, while the complete test/coverage gate proves Cabal resolves and runs every declared
test tool.

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
| `HarchWeb.Api`/`HarchWeb.Api.Endpoint` typed endpoints (buffered, URL-encoded form, multipart, and streaming request bodies) and closed route-family registry (`RouteFamily`/`combineRouteCodecs`/`apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition`) | Implemented (partial — see AC) | `examples/custom-api` and `examples/multipart-upload` are both migrated onto the route-family registry (2026-08-13), which also fixed a standalone-family not-found crash the migration surfaced (see the follow-up decision above). `ApiResponse` can now carry observability attributes/log entries (2026-08-13), closing the capability gap the custom-api migration surfaced. The now-unused compatibility `apiEndpointMiddleware`/`apiRouteEndpointMiddleware` (and the legacy `ApiEndpoint` table) were deleted 2026-08-13 (see the AK decision record), which closed AK's module-health signal too (`HarchWeb.Api.Endpoint` is 499 lines). `web-api` still hand-writes its own combined `AppRoute` dispatch and does not route `/api/*` through `HarchWeb.Api.Endpoint` at all; migrating it is the remaining AC follow-up work. |
| `HarchWeb.Api.Multipart` bounded streaming consumer, in-memory default, and native upload form | Implemented (partial — see AD) | Audited 2026-08-12 against AD's full text: storage ownership/cleanup, the in-memory default, case-insensitive media-type/boundary validation, preamble/header/body/declared-`Content-Length` bounds, the delimiter-sized scanning suffix, filenames-as-untrusted-metadata, and both scripts-enabled/disabled native-upload E2E cleanup proofs were already in place. `multipartLimitsMaxFieldCount`/`multipartLimitsMaxFileCount` closed the one confirmed gap (field/file counts were only bounded together via `multipartLimitsMaxParts`). On 2026-08-18, `UntrustedFilename` made the filename metadata boundary explicit and non-negative byte/item limit types made malformed negative configuration unrepresentable. Remaining open item: the unread-body/backpressure policy is a documented deferral to the WAI transport (`HarchWeb.Api.Multipart` stops reading after cleanup rather than draining), not an implemented drain mechanism — revisit only if a concrete backpressure problem is observed. See [multipart-upload](../examples/multipart-upload/README.md)'s `/native-upload` page (`App.MultipartUpload`) for the compiled, real-browser-tested demonstration. |
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
