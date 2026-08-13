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

### API transport helpers and the first shared endpoint boundary

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
`ApiBodyDecoder`) by the request's `Content-Type` (ignoring its parameters, case-insensitively), and
distinguishes an unsupported media type (`415`) and a malformed body (`400`) from a successful decode. The
endpoint's bounded body reader owns the separate oversized-body (`413`) outcome; `MissingContentTypePolicy` lets each
endpoint decide whether a missing header is rejected or resolved to a declared default.
`selectRepresentation` negotiates a response media type from a server-preference-ordered list and an
optional `Accept` header per RFC 9110 §12.5.1 (bounded three-decimal quality values, wildcards,
specificity precedence, `q=0` exclusion, and `406` when nothing is acceptable). `respondApiMatch` renders an `ApiMatchResult` into a
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
and `withMultipartBodyWith`/`withMultipartRequestBodyWith` to drive the scanner against a chunked body
(any `IO ByteString` source, or a WAI `Request` directly). `MultipartStorage` makes durable backends an
explicit application choice; the WAI helper uses the supplied `InMemoryUpload` adapter, retaining no file
bytes beyond `MultipartLimits`' max-file budget. A caller-supplied callback runs as soon as each part
finishes, before any later part (including a later file part) is read, so a caller can reject the whole body
— on an invalid CSRF field, say — before a later file reaches storage. File data is available only via an
opaque scoped upload: promote it deliberately to take ownership, or leave it for automatic cleanup after
success, rejection, or an exception.

`apiEndpointMiddleware` remains an additive compatibility helper; it is not the final route dispatcher.
New routes should use `apiRouteEndpoint` and place `apiRouteDefinition` in the application's
`RouteDefinition` table. That declaration names one method, typed query/header/cookie decoder, either no body
or exactly one bounded buffered body decoder, a domain-failure interpreter, and the response renderer.
The shared server dispatcher supplies the real WAI request only after it has selected the route and
enforced the table's method policy, so API declarations cannot create competing 404/405/`Allow`/`HEAD`/
`OPTIONS` behaviour. Fields are validated before a body reader runs; rejected fields return a stable
400 without consuming the body. A bounded buffered decoder maps oversized, unsupported-media, and
malformed input to 413, 415, and 400 respectively, and passes expected domain failures through the
endpoint's explicit interpreter. Endpoint handlers return a typed `ApiResponse`; the declaration's
non-empty `ApiResponseEncoder` list selects and encodes an acceptable representation, sends its
`Content-Type`, preserves application status and headers, adds or merges `Vary: Accept` when alternatives
exist, and returns 406 for an explicitly incompatible `Accept`. `ApiMultipartRequestBody` is the
streaming multipart request capability: it gives the handler a one-shot scoped consumer backed by an
application-selected storage adapter and `MultipartLimits`, preserving both duplicate-consumption rejection
and the multipart parser's typed failure. Additional streaming codec shapes and multipart storage policy
remain AC/AD follow-up work.

For a standalone WAI composition, `apiRouteEndpointAt` adds the endpoint's path to that same typed
declaration and `SomeApiRouteEndpoint` permits a heterogeneous table. `apiRouteEndpointMiddleware`
then derives `404` fall-through, `405` plus `Allow`, `HEAD`, and synthesized `OPTIONS` directly from
that table while invoking the declaration's own handler and single body consumer. This is the typed
replacement for the legacy target-plus-handler middleware; the legacy helper remains only for existing
applications while their endpoint declarations are migrated.

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
`urlEncodedFormBodyDecoder` decodes one `application/x-www-form-urlencoded` body with a field-count cap;
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

`runServerWithWaiMiddleware`/`withLocalTestServerForApplication` are the composition points that make
`apiEndpointMiddleware` usable against a real running (or locally test-served) application, not just a
bare `Wai.Application` in a unit test: `runServer` and `withLocalTestServer` are now defined as the `id`
middleware case of each. [multipart-upload](../examples/multipart-upload/README.md)'s `/native-upload`
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
its path has a declared endpoint, it selects the one whose declared method matches the real request
via `matchedApiRouteEndpointOrDie` — reachable only by a framework wiring defect there (an
inconsistent endpoint table between the codec and the definition), since every request method
`routeResponse` sees for a *declared* path has already been validated against this same family's
`routeMethods` by the shared dispatcher before `routeResponse` runs.

This closes the primitive gap named in "A closed route-family registry ... remain AC steps" above.

### Follow-up decision — standalone family not-found and the custom-api migration (2026-08-13)

**Fix, not a new decision: `apiRouteEndpointFamilyDefinition` must not call
`matchedApiRouteEndpointOrDie` for a path with no declared endpoint.** Migrating
`examples/custom-api` onto `apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` (below)
surfaced a real defect the primitive's own tests never exercised: `apiRouteEndpointFamilyCodec`'s
`notFoundRequest` resolves every unmatched path to the same synthetic `ApiPath ""` route, which has
no declared endpoint, but `apiRouteEndpointFamilyDefinition`'s `routeResponse` unconditionally
called `matchedApiRouteEndpointOrDie` for whatever path it was given — including that synthetic one
— throwing an uncaught `error` instead of rendering a `404`. Every landed test combined the family
with a catch-all second family via `combineRouteCodecs`, whose own not-found route absorbs the
not-found case before `apiRouteEndpointFamilyDefinition` ever sees the synthetic path, which is why
this went unnoticed until an application used the family **standalone** — precisely
`examples/custom-api`'s shape, since it has no page family to combine with. `web-api`'s route ADT
does not have this exposure even before any migration: its own `ApiNotFound`/`PageNotFound`
constructors are real, declared members of its route table with their own `RouteDefinition`, not a
codec-synthesized sentinel outside the table. Fixed by having `apiRouteEndpointFamilyDefinition`
check for a declared endpoint at its given path first and render an ordinary API `404` directly when
none exists, before ever calling `matchedApiRouteEndpointOrDie` — which keeps that function's
existing "wiring defect" framing accurate for its remaining, narrower call site. A new Unit test
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
the same underlying scanner and storage adapter either way. `App.App`'s `multipartUploadApplication ::
NativeUploadState -> Wai.Application` keeps its exact public signature (both the Unit and real-browser
E2E suites call it directly), now built via `simpleSite` + `buildSiteApplication` + `toWaiApplication`
instead of `apiEndpointMiddleware` wrapping a hand-written `notFoundApplication`. All 14 Unit tests and
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
`matchedApiRouteEndpointOrDie` still shares, updating both export lists, deleting the now-pointless
dedicated tests, and updating every doc that still describes the compatibility helpers as current) is
real follow-up work of its own, tracked under AK, not completed as part of this migration.

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
| `HarchWeb.Api`/`HarchWeb.Api.Endpoint` typed endpoints (buffered, URL-encoded form, multipart, and streaming request bodies), closed route-family registry (`RouteFamily`/`combineRouteCodecs`/`apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition`), and the compatibility `apiEndpointMiddleware`/`apiRouteEndpointMiddleware` | Implemented (partial — see AC) | `examples/custom-api` and `examples/multipart-upload` are both migrated onto the route-family registry (2026-08-13), which also fixed a standalone-family not-found crash the migration surfaced (see the follow-up decision above). `ApiResponse` can now carry observability attributes/log entries (2026-08-13), closing the capability gap the custom-api migration surfaced. No application in the repo still calls the compatibility `apiEndpointMiddleware`/`apiRouteEndpointMiddleware`; only `HarchWeb.Api`'s own facade and tests reference them, making their removal a real but unstarted follow-up (see AK). `web-api` still hand-writes its own combined `AppRoute` dispatch and does not route `/api/*` through `HarchWeb.Api.Endpoint` at all; migrating it is the remaining AC follow-up work. |
| `HarchWeb.Api.Multipart` bounded streaming consumer, in-memory default, and native upload form | Implemented (partial — see AD) | Audited 2026-08-12 against AD's full text: storage ownership/cleanup, the in-memory default, case-insensitive media-type/boundary validation, preamble/header/body/declared-`Content-Length` bounds, the delimiter-sized scanning suffix, filenames-as-untrusted-metadata, and both scripts-enabled/disabled native-upload E2E cleanup proofs were already in place. `multipartLimitsMaxFieldCount`/`multipartLimitsMaxFileCount` closed the one confirmed gap (field/file counts were only bounded together via `multipartLimitsMaxParts`). Remaining open item: the unread-body/backpressure policy is a documented deferral to the WAI transport (`HarchWeb.Api.Multipart` stops reading after cleanup rather than draining), not an implemented drain mechanism — revisit only if a concrete backpressure problem is observed. See [multipart-upload](../examples/multipart-upload/README.md)'s `/native-upload` page (`App.MultipartUpload`) for the compiled, real-browser-tested demonstration. |
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
