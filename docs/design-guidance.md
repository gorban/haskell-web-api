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

### Decision record — dependency-light test specification preprocessor (2026-08-22)

**Decision: extract the existing `SPEC` processor into the standalone
`test-spec-preprocessor` package, while retaining `TestCore.SpecPreprocessor` as a compatibility
facade.** The processor was previously an executable of `test-core`, with a Core-local copy for
Core tests.  That arrangement works only while every consuming test suite can depend on a package
whose library imports `hspec-expectations-match`.  Converting the matcher package's own tests to
`{-# SPEC #-}` exposed the cycle directly: its test component needed the processor, but each
available processor package had an enabled test component that depended back on the matcher.

The processor owns test-file module/prelude injection, a concern separate from application or
framework behavior, so this is an additive package rather than a parallel framework abstraction.
It has only filesystem and transformer dependencies, all test suites now use its executable, and
the old `TestCore` module remains an API-compatible re-export.  `spec-prelude=Test.Hspec` is a
per-source option for the standalone matcher spec; ordinary `SPEC` and `E2E_SPEC` behavior remains
unchanged.  This makes the convention available across the package graph without weakening either
the default test prelude or the matcher package's dependency boundary.

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

### Never mask a gate finding with an ignore pragma

A build warning, an HLint finding, or a coverage gap is a signal that something is wrong — in the
code, in the test suite, or in how a metric is being satisfied. Silencing the signal is not the
same as answering it. Do not reach for `{-# ANN foo ("HLint: ignore ..." :: String) #-}`,
`-Wno-...`, a `$!`/`seq`/`deepseq` added only to change what HPC attributes a tick to, or any other
suppression as the *standard* fix for a gate finding, including the CSE-sharing HPC artifact this
project's own memory has documented and reused as a fix many times. That reuse was a mistake to
generalize into standard practice: each occurrence is a case where the actual fix — deduplicating a
shared literal into one named binding, restructuring the code so the metric no longer misattributes
it, or extending the test to exercise the branch a different way — was available and not taken. An
ignore pragma is acceptable only as a last resort, after the restructuring options below have
genuinely been tried and shown not to apply, and it must say so: the comment next to it must name
what was tried and why it did not work, not merely restate that HPC/HLint disagrees with the code.

**Worked example — the fix that should have generalized, with a caveat found while generalizing it.**
The CAF-sharing gap documented under "API transport helpers and the shared endpoint boundary" above
(`[HarchWeb.RouteGet]` and `pure ()` literals shared into one CAF across two definitions) tried
`$!`/`seq` forcing first, found HLint correctly flagged it as forcing an already-WHNF value, and *did
not* suppress that finding — it deduplicated the literal into one named, exported binding
(`noApiRequestFields`) used by both call sites instead. That is the shape every future CSE-sharing gap
should take: find the shared literal or constructor reference and give it one name, rather than
forcing each call site and telling the linter to stop complaining about it.

**But naming alone does not reliably survive `-O2`, confirmed while closing task CB (2026-08-21).**
`noApiRequestFields`'s own type (`RequestCodec ()`, a `newtype`-wrapped function) has enough shape
that GHC keeps it as a real reference. A *trivial* nullary value — a bare data constructor, a `Text`
literal, `Just True` — does not: GHC's `-O2` optimizer inlines the named binding back to the literal
at each call site, silently reproducing the exact CSE-sharing gap the naming was meant to remove, with
compilation succeeding either way (the only way to catch it is rerunning the full coverage gate and
reading the per-line HPC HTML markup, not trusting that a build succeeded or extrapolating from one
prior fix). CB's own inventory hit this on `emptyFieldDefault`, `databaseConnectTimeoutSecondsKey`,
`derEncoding`, and `throttleCountsAsSuccess`/`noThrottleRecordingNeeded` — each needed a named binding
*and* a follow-up `$!` at the specific reference(s) still left unticked. Prefer naming first for types
with real shape (functions, `IO` actions, records); for a trivial nullary value referenced at 2+ call
sites, expect to need both, and verify with the full gate before considering it closed.

**Follow-up: complete.** Task CB in `TASKS.md` audited and fixed every existing ignore pragma in this
codebase against this rule (2026-08-21) — see its own completion note for the full list and the two
lessons (this one, and a companion one about deleting tautological `x == x` test assertions
regressing a *derived* instance's own coverage) discovered while verifying the fix, not just applying
it.

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

Correction (DJ, 2026-08-21): the "30 exports … under the 40-export threshold" framing above conflated
own-name count with consumer-facing surface. `tools/haskell-quality-report.sh`'s counter, before DJ's
fix, scored a `module X` re-export entry as 1 no matter how many names `X` carries — so
`HarchWeb.Security`'s real surface was 48 both before and after this split (30 − 1 + 19), and is 60
now that DE (2026-08-20) added a second re-export. The split's real justification was never "gets
under 40" — it was the call-graph-verified genuine decoupling above, which stands unchanged. See the
DJ decision record below for the general rule this falls under: a facade wholesale-re-exporting an
already-split, self-contained sibling module is exempt from the export-count threshold regardless of
the resulting raw number, per this document's own "non-facade public API" qualifier — narrowing such
a facade further would mean either re-merging the split (undoing real decoupling) or splitting the
re-exported cluster itself, neither of which this correction is asking for.

### Follow-up decision — DJ: fix the quality-report tool's export/arity counters instead of re-splitting facades that were never actually violations (2026-08-21)

**Decision: fix `tools/haskell-quality-report.sh`'s two counting bugs, then correct the three
decision-record numbers that depended on them, rather than re-splitting `HarchWeb.Security`,
`HarchWeb.Markup`, or `WebApi.Config` further.** DJ's finding was concrete: `module_export_count_for`
split each export-list line on commas and counted every resulting piece as one name, so a
`module HarchWeb.Security.RequestLimits` re-export line — which actually carries 19 names — scored
exactly the same as a single ordinary export. Every facade in this codebase (`HarchWeb.Security`,
`HarchWeb.Markup`, `WebApi.Config`, and `TestCore.Browser`) is built from exactly this pattern
(`module X, module Y, …` plus a handful of names of its own), so this bug understated every
facade's true consumer-facing surface, and three separate decision records (AL, Y, and this
document's own AL entry above) had cited the undercounted number as proof a module-health threshold
was satisfied.

Fixed the counter in two parts: `module_export_entries_for` now classifies each export-list entry as
either an ordinary name (`E:`) or a `module X` re-export (`M:`), and `print_module_health_reports`
resolves each `M:X` entry against `X`'s own resolved count (recursively, with a cycle guard, since
the same local-import-cycle machinery already in this script proves cycles are possible in principle)
when `X` is a module this scan also measured, falling back to counting it as a single opaque name
only when `X` isn't a local module the scan can inspect (an external package). Re-running the fixed
script against real code gives `HarchWeb.Security` = 60, `HarchWeb.Markup` = 65, `WebApi.Config` = 42
— all genuinely over 40, not under it as previously claimed.

None of the three needed re-splitting. AGENTS.md's module-health trigger names a **non-facade**
public API exceeding 40 exports; all three modules here are the facade case it already exempts — each
is a thin `module X, module Y, …` shell with zero or near-zero names of its own, and the modules
actually carrying the bulk of the surface (`HarchWeb.Security.RequestLimits`/`.ForwardedTrust`,
`HarchWeb.Markup.Attributes`/`.Elements`/`.Regions`/`.Syntax`, `WebApi.Config.Defaults`/`.Loading`/
`.Types`) are each individually well under the threshold on their own. `HarchWeb.Markup.Implementation`
(65 exports) and `WebApi.Config.Internal` (42 exports) are the two modules DZ separately worried might
themselves be non-facade violations, but both are Cabal `other-modules` — not part of the package's
public API at all, only reachable through the facade that re-exports them — so the non-facade rule
does not apply to them either. Re-splitting any of these three facades further would mean either
undoing genuine, call-graph-verified decoupling (AL, Y) to shrink the re-export count, or splitting an
already-cohesive sibling module apart purely to move its name count somewhere else — exactly the
"relocation, not a genuine shrink" pattern this document's AK and AL entries already warn against
chasing. AL's and Y's closure notes (and this document's own AL entry above) were corrected in place
to state the true numbers and the correct (facade-exemption) justification, not reopened.

CS's arity-metric finding was fixed alongside the export counter, in the same script.
`module_max_arity_for` matched any line shaped like `identifier … =` and counted whitespace-separated
tokens on the left of that `=`, excluding only lines containing `::`. Two failure modes followed
directly: an `import` line can itself contain a bare `=` (`(.=)`, aeson's `Value` combinator), so
`import Data.Aeson (Value (Null), …, (.=))` was read as a 10-argument equation; and a cons-pattern
clause written with spaced `:` (`stripSpecMode ('E' : '2' : … : rest) = …`) split into one token per
character and colon, reporting arity 17 for a genuinely one-argument function. Fixed by skipping
`import` lines outright and, wherever a top-level `::` signature is present, deriving arity by
counting depth-0 `->` occurrences in the signature instead of tokenizing the equation head at all —
the equation-head heuristic now runs only for the rarer case of a top-level binding with no preceding
signature. Re-running against the two files DJ's report cited confirms the fix: `TestCore.Browser.Protocol`
now reports arity 3 (was 10) and `TestCore.SpecPreprocessor` reports arity 3 (was 17), both matching
their real signatures. A multi-line signature's continuation lines are not merged in, which can still
undercount arity but — unlike the bug just fixed — can never fabricate a violation, which is the
failure mode that actually mattered here.

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

### Follow-up decision — BW: typed database operations reach OTLP at the response boundary (2026-08-19)

**Decision: extend the existing response and request-observability boundaries (option 1), rather
than reconstructing a second database-operation protocol from generic attributes.** `ResponseBody`,
`ProtocolResponse`, and `ApiResponse` now carry typed `HarchWeb.Database.DatabaseOperation` values;
the request executor attaches them to `RequestObservability`, and the OTLP exporter alone projects
them as client child spans. This is a small, general framework primitive: page and typed API
responses already own request-scoped diagnostics, and all database adapters need identical parent/
child semantics. Generic attributes stay generic—even attributes named `db.*`—so order, adjacency,
or unrelated interleaved attributes cannot silently create or discard a span. The database-system
identifier remains extensible `Text`, not a framework newtype or closed enumeration: application
adapters are deliberately open, and wrapping or closing that identifier would not validate it or
prevent a useful invalid state. Focused OTLP capture tests cover multiple operations, timing and
untimed operations, and generic interleaved attributes; the full coverage gate is required proof.

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

### Follow-up decision — DF: HTTPS-redirect authority stops trusting the request's own Host header (2026-08-20)

**Decision: extend `RequestPolicyConfig` with a canonical `httpsRedirectAuthority` field (option 1,
small and general); do not have the framework introspect `AcmeConfig`/TLS bind-plan config to guess
a domain.** `requestRedirectAuthority` echoed the request's own `Host` header into the 308 upgrade's
`Location`, an open redirect an attacker (or a caching intermediary) fully controls by setting that
header on a plaintext request. The task's own text suggested the framework already had a usable
domain list on `AcmeConfig`/`ManualTlsBindPlan`; checking found `ManualTlsBindPlan` carries no domain
field at all, so that path does not generalize. `HarchWeb.Security.requestRedirectLocation` now never
reads the request's `Host` header — it renders `httpsRedirectAuthority` (with the existing
`httpsRedirectPort` rewrite) or does not redirect at all when unset, rather than falling back to the
untrusted header.

The framework-level default (`WebApi.Config.Internal.defaultHttpsRedirectAuthority`) derives the
authority from this app's own unique HTTPS listener host, mirroring `defaultHttpsRedirectPort`'s
existing gate (an HTTP listener must also be present). That default is `Nothing` for a deployment
behind a TLS-offloading reverse proxy, which declares no local HTTPS listener at all — exactly the
shape `REDIRECT_HTTP_TO_HTTPS` most needs, per the existing "TLS-offload deployments" test. Rather
than add a second, redundant config key for that case, `WebApi.App.buildRuntimeApp` extends the
already-required `PUBLIC_BASE_URL` setting (already the canonical external URL used for email
links) to also supply the redirect authority, overriding the listener-derived guess when it parses.
No new abstraction and no new required setting were added; both existing boundaries — the framework's
request-policy record and web-api's own composition-root override point — were extended in place.

### Follow-up decision — DE: forwarded-header trust becomes a property of the peer, not a global flag (2026-08-20)

**Decision: add `HarchWeb.Security.ForwardedTrust` as a new sibling module (option 1, small and
general, genuinely disjoint concern) implementing exactly the task's own sketched
`ForwardedHeaderTrust`/`CidrBlock` shape; do not have the framework track hop counts or IPv6.**
`trustForwardedHeaders :: Bool` let any client spoof `X-Forwarded-For`/`-Proto`/`-Prefix`/`Forwarded`
regardless of who actually connected, poisoning `client.address` observability and letting a client
downgrade its own connection's HSTS/redirect behavior by simply sending a header. The field is now
`forwardedHeaderTrust :: ForwardedHeaderTrust`, and the framework's three header-trust gate functions
in `Security.hs`, plus `WebApi.Route.requestContextFromWaiRequest` (a second, easy-to-miss choke
point for `X-Forwarded-Prefix` that lived in web-api's own composition, not the framework), all check
the request's actual TCP peer (`Wai.remoteHost`) against the configured CIDR list — never the
client-claimed header alone. A Unix-domain-socket peer is always trusted once forwarding is enabled
at all: unlike an IP, it cannot be spoofed by a remote client, since only a process with filesystem
permission on that exact socket path can connect — a stronger guarantee than any CIDR check gives,
confirmed against a pre-existing test proving that deployment shape must keep working. IPv6 peers are
not matched by any CIDR block (the existing codebase's own `socketAddressText` is already IPv4-only);
extending to IPv6 is a natural, separately-scoped follow-up if a deployment needs it, not silently
assumed here.

**Also recorded — a genuine GHC 9.14.1 `-O2` HPC instrumentation quirk, not a missing test.**
`ForwardedHeaderTrust`/`CidrBlock` initially used `deriving (Eq, Show)`. Their derived instances'
never-overridden `/=`, `show`, and `showList` default methods showed permanently unticked regardless
of test coverage, and — unlike the project's established CSE-sharing artifact (see the coverage-gate
memory) — adding more covering tests moved *which* sub-expression showed unticked rather than closing
the gap, across several full rebuilds. The `.mix` file (not guesswork) showed why: GHC attributes each
unoverridden class default method to its own coverage box at the bare `instance ... where` line, and
the optimizer specializes/inlines a call to that method away before it registers as its own tick —
while a real `/=` comparison, a bare `show`, and a `show` of a list (to reach `showList`) reliably
fixed it once *every* default method had its own genuine call site. Both instances are now
hand-written; ordinary user-defined functions do not have this problem and are covered normally. This
is a third, distinct technique from the already-documented `$!`-forcing and extract-and-test ones —
recorded in the `coverage_gate_haskell_web_api.md` memory so a future session facing an
instance-method coverage gap does not misdiagnose it as a missing test either.

### Follow-up decision — DG: delete the never-wired native ACME protocol client rather than hardening it (2026-08-20)

**Decision: delete `Acme/Protocol/{Client,Workflow,Types,Decode}.hs`, `Acme/KeyMaterial.hs`,
`Acme/OpenSsl.hs`, and `Acme/Crypto.hs` outright, rather than replacing their `openssl`-subprocess
calls with in-process crypto.** DG's finding — RSA-4096 keygen, RS256 signing, and SHA-256 hashing
done by shelling out to `openssl` resolved through `$PATH`, with the raw command runner exported
publicly — was accurate as a description of the code, but investigating where that code is actually
used (prompted by a direct question: is any of this necessary, since ACME already works without it)
found it has **zero production callers**. `AcmeConfig` (`Server/Config.hs`) requires a
`CertbotConfig` with no alternative constructor; the only cert-acquisition path any running server
takes is `Acme/Certbot/Runtime.hs`'s `certbot` subprocess, which owns its own crypto entirely outside
this codebase. Commit history confirms this was never live: the squash-merge that created `HarchWeb`
already lists "Remove in-process ACME backend" among its own history, and a later, unrelated
pure-move refactor pass relocated the already-dead code into today's dedicated modules without ever
wiring it up — making it look more current and load-bearing than it was.

This is squarely the "add the primitive to the framework" vs. "flag and stop" fork the missing-
capability protocol above describes, except for a fourth case the protocol doesn't name directly:
the code already exists, so the real choice is finish-and-hardenit vs. delete-it. Finishing it means
solving CSR generation — a PKCS#10 `CertificationRequest` as raw ASN.1 DER — which no Hackage package
in this project's dependency closure builds from scratch (`x509`/`crypton-x509`, already available
transitively via `tls`, only model and parse certificates); doing it correctly means hand-writing
ASN.1 DER encoding, a genuinely error-prone, security-sensitive investment in a backend nothing
calls. Deleting closes the actual attack surface ($PATH-resolved `openssl`, publicly exported) more
completely and far more cheaply, and the task's two smaller findings (raw stdout/stderr spliced into
a `userError`; unescaped domain interpolation into an openssl.cnf) disappear with the code they lived
in rather than needing separate fixes. `HarchWeb.Acme`'s module doc now states plainly that ACME is
always certbot-backed by design, closing the door on a reader assuming a native alternative is
half-built and safe to extend. If a certbot-free ACME backend is ever wanted, it should be scoped and
built fresh against current requirements — at which point the CSR-encoding question needs a real
answer regardless, so nothing already written here would have been reusable as-is.

`HarchWeb/Acme/Json.hs` was investigated and kept: although built for the now-deleted protocol
client, it also backs `HarchWeb.Observability.Otlp.Wire`'s live OTLP JSON encoding. DH (a separate,
still-open task) targets this exact file's hand-rolled `ReadP` parser; its own findings remain real
and its task text was updated to stop pointing at the now-deleted ACME-response decoder types.

### Follow-up decision — DH: delete `Acme/Json.hs`'s parser rather than fixing it, once DG proved it dead too (2026-08-20)

**Decision: delete the whole `ReadP`-based JSON parser, `JsonValue`, and every field accessor from
`Acme/Json.hs`, rather than fixing the four concrete defects DH found in them; keep only the three
byte-builder encoders (`jsonArrayBytes`, `jsonObjectBytes`, `jsonStringBytes`), and make the module
directly importable (`exposed-modules`, not re-exported through `HarchWeb.Acme`) so the package's
own tests can still reach them.** DG's note above assumed `Otlp/Wire.hs` used `Json.hs`'s
`JsonValue`/field-helper reading side; tracing its actual import list
(`import HarchWeb.Acme.Json (jsonArrayBytes, jsonObjectBytes, jsonStringBytes)`) showed that
assumption was wrong — OTLP export only ever used the encoder half. With DG having already deleted
every parser caller, `parseJsonValue` and everything under it were exactly as dead as the code DG
removed, for the same reason: fixing partial `decodeUtf8`, unbounded materialization, ambiguous-parse
enumeration, and missing surrogate-pair handling in a parser nothing calls would have hardened
unreachable code, not closed a real gap. Deleting it removes DH's findings outright, the same
resolution DG reached, and also closes DH's "exported as public API through the `Acme` facade"
finding, since the facade no longer mentions `Json.hs` at all.

The one wrinkle deleting the parser exposed: a Cabal test-suite component cannot see a library's
`other-modules`, so once the facade re-export (the very thing DH flagged) was gone, nothing let
`AcmeSpec.hs` reach the three encoders — and no other test in the repository drives the real OTLP
HTTP-export path, so simply deleting the test would have silently regressed coverage to 0% on code
`Otlp/Wire.hs` genuinely depends on. This is the missing-framework-capability fork in miniature:
rather than mocking OTLP export or reaching into a hidden module some other way, `HarchWeb.Acme.Json`
moved to `exposed-modules` on its own — not through the `Acme` facade, so nothing implies it is ACME
functionality — trading a small, honest, directly-named public surface for keeping a real coverage
source for live code. This is a smaller and more legible exposure than what DH originally flagged:
previously the *entire* parser, `JsonValue`, and every accessor were re-exported through a facade
that implied they were curated ACME functionality; now three leaf functions are importable by their
own name, from a module whose Haddock says plainly it exists only for the test suite and for
`Otlp/Wire.hs`.

### Follow-up decision — DK: make `toWaiApplication` self-gating instead of adding a gated variant (2026-08-21)

**Decision: bake AF's concurrency admission gate directly into `toWaiApplication` — "the public WAI
adapter" itself — rather than adding a second, gated entry point alongside the existing ungated
one.** DK's finding traced cleanly: `HarchWeb.Server.Runtime` and `HarchWeb.Server.LocalTest` both
already composed `concurrencyLimitedMiddleware` around `toWaiApplication` correctly (AF's own fix
covered both), but `toWaiApplication` is also the *only* function the public `HarchWeb`/
`HarchWeb.Server` facade exposes for turning a typed `Application` into a `Wai.Application` at all —
`concurrencyLimitedMiddleware` itself lives in `other-modules`, unreachable by application code. Both
shipped examples that build their own exported `Wai.Application` (`examples/custom-api`,
`examples/multipart-upload`) necessarily went through bare `toWaiApplication`, and
`multipart-upload`'s real `app/Main.hs` runs the result via `Warp.run 8080` directly, entirely
outside `HarchWeb.Server.Runtime` — a configured `RequestConcurrencyLimit` would have been silently
inert there, and there was no public alternative constructor an application could have reached for
instead, even if the gap had been noticed.

This is the "extend an existing boundary before adding a parallel one" rule applied to a case where
the "boundary" in question is a single function's safety guarantee rather than a whole dispatch
abstraction: the tempting alternative — leave `toWaiApplication` as-is and add a new
`toGatedWaiApplication` (or similar) that composes the gate — would have reproduced the exact defect
under discussion, just with a different name to forget. None of the rule's three conditions for a
parallel surface held (no existing caller's meaning would change — the gate is a no-op for every
current test and example, none of which configures a `RequestConcurrencyLimit` today; it is not a
disjoint concern, it is the same request lifecycle `toWaiApplication` already owns; and DK's text
does not authorize a new surface), so the default applied: extend `toWaiApplication` itself.

Implementation consequence: `toWaiApplication`'s type changed from
`Application route action context -> Wai.Application` to `... -> IO Wai.Application`, since the
gate's in-flight-request counter is real mutable state (an `IORef`) that must be allocated once per
running server and shared across every request that server handles — allocating it fresh per request
(which calling it from inside a per-request `Wai.Application` closure would do) would make the
counter always read zero and never actually limit anything. `Runtime.hs`'s and `LocalTest.hs`'s own
gate composition became redundant and was removed; both now call `toWaiApplication` once at server
startup instead of once per request, which is also strictly more efficient than the previous
per-request re-construction (harmless before only because the ungated function had no per-call cost).
Every caller of `toWaiApplication` — the two examples and roughly 150 test call sites across
`HarchWebSpec.hs`, `WebApiSpec.hs`, `SiteSpec.hs`, `AppSpec.hs`, `FacadeSpec.hs`, and
`MultipartUploadSpec.hs` — needed a mechanical update for the new `IO` return type; the overwhelming
majority fit the single `performWaiRequest (toWaiApplication X) request` shape and were fixed by
changing each file's local `performWaiRequest` helper to accept `IO Wai.Application` and bind it
internally, leaving the call sites themselves untouched. A handful of call sites that pass
`toWaiApplication X` to something other than `performWaiRequest` (`startManualTlsRuntimeServerWithStarter`,
`startHttpRuntimeServerWithStarter`, a raw `Wai.Application`-shaped lambda for
`withLocalTestServerForApplication`) needed an explicit `<-` bind instead. No new tests were needed:
the existing suite's extensive `toWaiApplication` coverage and AF's own dedicated gate tests already
exercise every changed branch, confirmed by a genuine 100% coverage re-run rather than assumed.

### Follow-up decision — AY: add `connect_timeout` now, defer `sslmode=require` to a deployment decision this codebase can't make alone (2026-08-21)

**Decision: implement `DATABASE_CONNECT_TIMEOUT_SECONDS`/`connect_timeout` unconditionally, but do
not default `sslmode` to `require`.** AY's own text proposed both changes as one step ("Add
`sslmode=require connect_timeout=…` now"). Investigating before implementing split them: this
project's own local/CI PostgreSQL — the stock `postgres:17` image — has `ssl=off`
(`SHOW ssl;` confirmed it directly), so `sslmode=require` would refuse every connection this
project's own test suite and local dev setup make. That is this document's "when implementation
hits a missing framework capability" case 2 turned around — the *workaround* asked for by the task
(hardcoding `require`) is the one that would silently substitute a materially different property
(the app refuses to start against any non-TLS Postgres, including every environment this repository
ships with today) for what the task assumed (a security hardening that just works). Provisioning
TLS on a Postgres server is infrastructure this codebase does not control; enforcing it is a
deployment decision, not a Haskell code change, so it was left unmade rather than picked
unilaterally — matching AW's precedent of naming a real blocker instead of shipping a workaround
with unstated consequences.

`connect_timeout` has no such dependency (a client-side setting, identical behavior regardless of
server TLS) and directly closes the concurrency-starvation half of AY's finding ("a wedged server
pins a request thread indefinitely, and the AF concurrency gate then 503s everyone"), so it shipped
now: a new `databaseConnectTimeoutSeconds :: Int` field on `DatabaseConfig`, sourced from
`DATABASE_CONNECT_TIMEOUT_SECONDS` (default `10`, following the existing committed-default pattern
every other `DATABASE_*` field uses) and applied to every libpq conninfo string
`runtimeConnectionString` builds. The migration-side `WEB_API_MIGRATION_DATABASE_*` parser needed
the same field populated (one shared `DatabaseConfig` record), but supplied it as a hardcoded
constant rather than a second required environment variable: that parser's psql-subprocess path
doesn't read the field at all, and a one-shot batch migration command has no concurrent-request
thread for a wedged connection to starve. Threading the timeout into the psql subprocess environment
too (`PGCONNECT_TIMEOUT`) was considered and rejected — `passwordEnvironment` is shared by every
psql invocation, and widening it would have broken several `Integration/WebApiSpec.hs` tests that
assert its exact environment against real subprocess runs, for a code path with no starvation risk
to close in the first place.

A genuine coverage-gate CSE-literal gap surfaced while closing this out:
`parseConnectTimeout = parseNonNegativeInt "DATABASE_CONNECT_TIMEOUT_SECONDS"` left the string
literal permanently unticked despite the declaration itself running on every config-parsing test,
the same documented pattern this memory/document has already closed twice before (AZ, DE) — `$!`
at the literal's call site (`parseNonNegativeInt $! "DATABASE_CONNECT_TIMEOUT_SECONDS"`) fixed it
immediately, confirmed by re-running the full coverage gate rather than assumed.

Follow-up: AX already identified that genuine migration atomicity needs a persistent-connection
Postgres runtime replacing the current per-statement `psql` subprocess model; AY's own deferred
connection pool is the natural companion piece to design alongside it, per AX's note. `sslmode`
becomes safely defaultable to `require` only once that follow-up (or a separate deployment change)
actually provisions TLS on the Postgres server(s) this project deploys against — until then,
defaulting it on would be optimizing for a security property this project cannot yet exercise in
its own test suite.

### Follow-up decision — AY: the connection pool, kept separate from AX's migration runtime (2026-08-21)

**Decision: ship the runtime-query connection pool now, as its own change, rather than waiting to
design it together with AX's migration runtime as the note above suggested.** Revisiting that note
before building: the two are not actually coupled at the implementation level. AX's blocker is that
migrations run through one `psql` subprocess per statement, so there is no single persistent
connection to hold a transaction or an advisory lock across statements — fixing that means replacing
the subprocess model outright, independent of anything the runtime query path does. The runtime
query path already used `Database.PostgreSQL.LibPQ` directly (`runRuntimeParameterizedRowsQuery`
and siblings), just via `bracket (LibPQ.connectdb …) LibPQ.finish` per call; pooling that is a
refinement of an existing boundary (`WebApi.Postgres.Runtime`), not a new one, and needed no
decision that also constrains AX's separate psql-to-libpq migration. Waiting would have blocked a
real, closeable half of AY on an unrelated, larger, still-unscoped task.

**Shape: a new `WebApi.Postgres.Pool` (bounded, lazy, explicit-prop) alongside the existing
`WebApi.Postgres.Runtime`, not folded into it.** `PostgresPool` holds live libpq connections in an
`idle :: TVar [Connection]` plus a `live :: TVar Int` count bounded by capacity; `acquirePooledConnection`
reuses an idle connection or, under capacity, reserves a slot and opens a fresh one, blocking via STM
`retry` at capacity; `releasePooledConnection` checks `LibPQ.status` and either returns a healthy
connection to idle or discards a broken one and frees its slot, so a connection that dies gets
replaced by the next acquirer rather than poisoning the pool. Connections are opened **lazily** —
`newPostgresPool` only allocates two `TVar`s, never connects — so building a pool against an
unreachable database still succeeds, and only the first query against it fails; this preserves the
exact behavior `Integration.WebApiSpec`'s "maps runtime PostgreSQL connection failures... without
shelling out to psql" test already asserted; making pool construction eager and fail-fast at startup
would have been a legitimate but different design with its own tradeoffs, changing today's "database
down at startup still serves non-DB routes" behavior — not made unilaterally here. The pool is
threaded as an explicit prop from `WebApi.App`'s `runWithConfig` through `buildRuntimeApp` and
`buildRuntimeAccountWorkflow`, one pool shared by every repository builder, per BX's precedent above
(explicit-prop caching over a second `unsafePerformIO`/`NOINLINE` global) rather than joining
`otlpExportQueue`/`otlpHttpManager`'s existing shape. Every `buildRuntimePostgresX`/
`buildRuntimePostgresXWithRunner` pair's `WithRunner` half was generalized from a `DatabaseConfig`-
typed runner to a type-variable-typed one (`(source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
source -> X`) so the plain builder could switch to `PostgresPool` while every existing fake-runner
test (`buildRuntimePostgresXWithRunner (\_ _ _ -> pure result) postgresTestConfig`) kept compiling
unchanged, `source` simply inferring back to `DatabaseConfig` there. `DatabaseConfig` itself gained
`databasePoolCapacity :: Int` (`DATABASE_POOL_CAPACITY`, default `10`), following
`databaseConnectTimeoutSeconds`'s exact pattern including its migration-path precedent: the
`psql`-subprocess migration runner has no pool to size, so `WebApi.DatabaseSetup.parseDatabaseSetupConfig`
supplies a hardcoded `migrationDatabasePoolCapacity = 1` rather than a second unused required env var.

**Three genuine coverage gaps surfaced, all the documented "bare variable as a direct call argument"
HPC artifact, none needing an ignore pragma — confirmed by actually running HLint, not assumed by
analogy.** `writeTVar (poolIdleConnections pool) remainingIdleConnections` (a pattern-bound list
tail), `buildRuntimeAccountWorkflow pool environmentConfig` (`pool` referenced twice in one `let`,
crediting only the first use), and `parsePositiveInt databasePoolCapacityKey` (the same
named-CSE-literal gap `databaseConnectTimeoutSecondsKey` hit) all needed `$!` at the uncredited
reference. The refinement to this document's own worked-example precedent: forcing a *bare
identifier* (a pattern-bound variable, a function parameter, or a same-module named literal) is not
automatically an HLint "Redundant $!" case the way forcing a literal or a fully-applied constructor
is — running `hlint --language=ImportQualifiedPost` against all three sites, and against the existing
`parseConnectTimeout = parseNonNegativeInt $! databaseConnectTimeoutSecondsKey` with its own ignore
pragma stripped, returned "No hints" in every case. HLint cannot prove a bare identifier resolves to
a value already in WHNF without cross-declaration analysis it does not perform, so it does not flag
forcing one — meaning an ignore pragma here would itself be an unverified, unnecessary suppression,
exactly what the never-mask-a-gate-finding rule warns against. All three `$!` additions ship with a
plain comment explaining the HPC gap; none carries `{-# ANN ... "HLint: ignore Redundant $!" #-}`.

### Follow-up decision — CK: nested per-constructor detail records close `-Wpartial-fields`, following each type's own sibling convention (2026-08-21)

**Decision: wrap the fields specific to each flagged constructor in its own single-constructor
nested record, rather than a blocklist, a `DuplicateRecordFields` merge, or leaving the flag
unset.** CK's own deferred-scope note already named the two remaining flagged types
(`HarchWeb.Server.Config`'s `TlsCertificateSource`/`TlsStartupMode` and `WebApi.Page.Model`'s
`ProfilePageModel`) and the mechanical cost (restructuring plus ~100 pinned derived-`Show` string
assertions across two multi-thousand-line files); this closes both.

**Field-name collisions were resolved by matching each file's own already-demonstrated convention,
not by reaching for `DuplicateRecordFields`.** `ProfilePageModel`'s four constructors shared bare
names (`profileHeading`, `profileSignInAction`, …) that would collide once split into four separate
record types. `HarchWeb.Server.Config` and `WebApi.Config.Internal` both already carry a
`{-# LANGUAGE DuplicateRecordFields #-}` pragma, which looked like precedent for keeping the bare
names and letting the extension disambiguate by usage-site type inference — but checking rather
than assuming: neither file has ever actually declared two types sharing a field name (confirmed
by grep), so the pragma is vestigial in both, not a demonstrated pattern to extend. `WebApi.Page.Model`
itself settles the question instead: `HomePageModel`, `SecondPageModel`, and `NotFoundPageModel` —
sibling types in the exact same file — already give their own `heading`/`summary`-shaped fields a
type-specific prefix (`homeHeading`, `secondHeading`, `notFoundHeading`). `ProfilePageModel` was the
one type in that file that hadn't followed suit, which is what let its fields go unprefixed long
enough to become genuinely partial. The fix applies that file's own dominant convention to its one
outlier (`signedOutProfileHeading`, `pendingProfileHeading`, …) rather than introducing
`DuplicateRecordFields` as new precedent on the strength of an unused pragma. `TlsCertificateSource`
needed no such renaming: `ManualTlsCertificateFiles`'s and `SharedTlsCertificateFiles`'s field sets
are already disjoint.

**`TlsStartupMode`'s `AwaitCertificateFiles` stayed positional rather than gaining a nested record.**
Its one field, `certificateWaitTimeoutSeconds`, has zero external accessor use (confirmed by grep,
not assumed) — every caller either constructs or pattern-matches it, never calls it as a bare
function. A single-field record with no reader beyond its own declaration would exist solely to
satisfy the linter, so it became a plain positional `Maybe Int` instead — the same choice CK's own
first `-Wpartial-fields` fix already made for `HarchWeb.Api.Endpoint.Internal`'s `ApiRequestBody`
GADT, whose one record-syntax constructor also had zero external accessor use.

**Nesting reopened two derived-instance coverage gaps this session had already named as a general
pattern, in a new shape.** BZ's and CB's own lessons (a deleted tautological `x == x` losing a
derived `Eq`'s default `/=` credit) covered the *within-one-type* case. Nesting surfaced a second
variant: a nested type's own `deriving (Eq, Show)` is credited only when something calls `==`,
`/=`, `show`, `showsPrec`, or `showList` **directly at that nested type**, not merely reached
indirectly through the outer sum type's own derived instances delegating to it — even when a test
already exercises the outer type exhaustively. Verified directly against the coverage gate, not
assumed: fixing required, for each of the six new nested types, all of (a) an equality assertion
against a same-value/different-construction pair, (b) an inequality assertion against a
same-shape/different-field pair, (c) a direct `show`, (d) a direct `showsPrec` at a precedence high
enough to require parenthesization, and (e) a direct `show` of a one-element list (`showList`) —
five distinct HPC-tracked boxes per type, discovered one at a time by re-running the coverage gate
after each partial fix rather than guessed in one pass.

### Follow-up decision — BA: give a taken username its own outcome, and target the conflict the insert is actually protecting (2026-08-21)

**Decision: replace `AccountStore.createPendingAccount`'s `Bool` result with a three-way
`CreatePendingAccountOutcome`, target the insert's `ON CONFLICT` at the email constraint
specifically, and check username availability up front with a second query rather than one atomic
statement.** BA's own 2026-08-15 note already scoped this precisely and correctly, including
naming the exact three-way type, the targeted conflict clause, and the ~20 test call sites it would
touch; this follow-up implemented that scope as written rather than re-deriving it. The one design
choice BA's note left open — one atomic CTE-union query vs. two round trips with a documented race
— was resolved in favor of two round trips, matching the note's own "upfront username-availability
check" phrasing: a single query returning a distinguishable outcome for three cases (created /
email taken / username taken) is expressible in Postgres, but only via a more complex CTE-and-UNION
shape whose correctness would have been materially harder to verify by reading, for a security
task where getting the query subtly wrong is worse than a documented, narrow race between two
concurrent registrations for the same available username.

The response-layer half of this decision is the more interesting one: a taken username gets its
own distinguishable 422 response, while a taken email address keeps the existing byte-identical
`Right _` branch shared with a genuine registration. These look like the same category of "does
this outcome leak information" question that the 2026-08-15 fix answered by hiding the email case,
but they are not — a username is what a user picks and is commonly checked for availability
directly during signup on far more security-conscious services than this one; an email address is
what identifies a specific person's account, and confirming one is registered is a privacy/
enumeration concern in a way confirming a *username* string is taken is not. Treating them the same
(hiding both) would have left BA's own stated goal — restoring a legitimate user's ability to
recover from a taken-username collision — permanently unmet for no security benefit; treating them
differently, as implemented, closes both findings correctly rather than trading one for the other.

`CreatePendingAccountOutcome` deliberately has no `deriving` clause, matching `RegistrationResult`'s
own existing style in the same module: tests use pattern-match predicates
(`\case PendingAccountCreated -> True; _ -> False`) instead of `==`, sidestepping the derived-
`Eq`/`Show`-under-HPC coverage gap this session already root-caused and documented twice (DE, and
the memory it lives in) rather than needing a third encounter with it. A smaller instance of this
session's *other* documented coverage-gate pattern — a CSE-shared bare atom losing its own HPC tick
— surfaced anyway, this time on a data constructor (`AccountStoreUnavailable`) rather than a string
literal, in the new upfront-check code path; `$!`-forcing it at the same argument position the
technique already covers closed it immediately, confirmed by a genuine coverage re-run.

### Follow-up decision — BR: validated newtypes plus `IsString`, not a blocklist or a bare `Text` parameter (2026-08-21)

**Decision: introduce `DataAttributeSuffix` and `SafeUrl` as validated newtypes with `IsString`
instances, rather than either a runtime blocklist check inside `dataAttribute`/`href` or leaving
their parameters as bare `Text`.** This is the extend-vs-new-abstraction call from the "Design
decisions before you build" framework: `HarchWeb.Markup.Internal`'s existing `AttributeName`
newtype already had the right shape for this (a validated wrapper around `Text`) but no validation
at all, so the fix extends that existing boundary — giving the two *public* construction paths
(`dataAttribute`/`dataFlag`'s suffix, `href`'s URL) their own validated types — rather than adding
a second, parallel checking mechanism next to it. A runtime blocklist (reject `javascript:`,
`data:`, `vbscript:`, …) was rejected for `SafeUrl` specifically: a blocklist can miss an obscure
dangerous scheme a browser will still execute, while an allowlist (relative reference, or
`http`/`https`) cannot silently regress the same way — BR's own task text already named this
tradeoff correctly.

The `IsString` instance is the piece that made this tractable at the ~30-call-site scale this
change touches: every existing call site already writes `dataAttribute "harch-action" ...` or
`href "/second"` as an `OverloadedStrings` literal, so giving each newtype `fromString = fromMaybe
(error ...) . mkX . Text.pack` lets every such literal keep compiling completely unchanged, with
the validation still running (`OverloadedStrings` resolves through this instance at compile-adjacent
time, against text the application author wrote themselves) and a malformed literal caught the
first time that page renders — the same failure mode any other `OverloadedStrings`-literal type
already accepts. A caller building either value from genuine runtime `Text` (a redirect target, a
user-supplied link) must go through the explicit `Maybe`-returning `mkDataAttributeSuffix`/
`mkSafeUrl` and handle rejection — the untrusted-input resource-ownership principle applied to
"which text is allowed to bypass validation": never runtime/caller-supplied text, always exactly
the literal the application author wrote at the call site.

This surfaced a genuine framework-capability gap mid-task, handled per the protocol's "add the
primitive" fork: `HarchWeb.Markup.Quasi.Lowering`'s `[harch| ... |]` EDSL lowers markup-source
string literals through its own `textLiteral` helper (`AppE (VarE 'Text.pack) ...`), which
bypasses `OverloadedStrings`/`IsString` entirely — it is TH-generated code building `Text` directly,
not going through GHC's literal-defaulting mechanism. Left unfixed, every `data-*`/`href` literal
written inside quasiquoted markup (the majority of real call sites in this codebase) would have
failed to compile despite the `IsString` design being otherwise correct. Fixed with a parallel
`fromStringLiteral` helper (`AppE (VarE 'fromString) ...`) routed through the `data-*`-suffix and
literal-`href` lowering paths specifically, rather than changing `textLiteral`'s behavior for every
other attribute (which still wants plain `Text`, not a validated type).

`HarchWeb.Controls.pageLink`'s route-render parameter changed from `route -> Text` to
`route -> SafeUrl`, pushing the safety proof to the type that actually renders a route into an
`href`. Both real call sites that build one (`App.Routes.twoPageNavigationPath`,
`WebApi.Page.Building.buildCallToAction`) render exclusively from a closed, always-safe typed route
table and can show by construction that `mkSafeUrl` never actually rejects their output — a rejection
there could only mean a route itself was defined to render an unsafe URL, a programming mistake in
that renderer, not a runtime condition either function's own callers need to handle. Rather than
duplicate a `fromMaybe (error ...) . mkSafeUrl` inline in both places, this need (now shared by two
call sites with identical shape) was itself the extend-vs-new-abstraction question one level down:
a new framework-level `HarchWeb.requiredSafeUrlOrDie :: Text -> Maybe SafeUrl -> SafeUrl` was added
once, matching the exact shape and naming convention `WebApi.Login`'s existing
`requiredPasswordHashOrDie` already established this session for "assert a value that's provably
always present, or crash with a named diagnostic" — and both call sites use it instead of hand-
rolling their own.

Two rounds of genuine coverage gaps surfaced closing this out, both already-documented patterns
applied to new shapes rather than new problems. First: `requiredSafeUrlOrDie`'s `context` argument
is only demanded on its (never-taken, by construction) `error` path, so the `Text.concat`-shaped
diagnostic-message expression at each call site stayed a permanently-unticked, unforced thunk under
HPC in every real run — closed with the established `$!`-forcing technique at each call site
(`(requiredSafeUrlOrDie $! contextExpr) (mkSafeUrl renderedPath)`), not by trying to make the
`error` branch reachable. Second: `DataAttributeSuffix`/`SafeUrl`'s `deriving (Eq, Show)` showed
"top-level declarations" gaps despite exhaustive `==`/`/=`/`show`/`show [x]` exercises covering most
of it — the residue needed an explicit *equal-value* `==` exercise (`x == x`) alongside the
already-present *unequal-value* `/=` exercise, confirmed by reading the per-line HPC HTML markup
rather than assumed. Both `fromString`'s own `error` fallback (the invalid-literal path) needed its
own direct test forcing it via a deliberately malformed `OverloadedStrings` literal at the type,
matching the `requiredXOrDie` extract-and-directly-test pattern rather than trying to make production
code take that branch.

### Follow-up decision — BS: quoted `Name`s for framework identifiers, and breaking the module cycle that stood in the way (2026-08-21)

**Decision: resolve every framework identifier the quasiquoter splices through a quoted `Name`
('`Impl.foo`) against `HarchWeb.Markup.Implementation`'s own compile-time scope, not `mkName`
resolved dynamically at the splice site — and restructure the module graph first, since the direct
fix was blocked by a genuine import cycle.** This is the extend-vs-new-abstraction call applied to
the module's *own* documented fix ("Lowering.hs:367 (hygienic, correct) … Lowering.hs:358
(unhygienic)"): `textLiteral`'s `'Text.pack` was already the correct pattern sitting one function
away; the fix is extending that same quoted-`Name` approach to every other framework identifier the
module splices, not inventing a new resolution mechanism.

Implementing it hit a missing-framework-capability gap the task's own text did not anticipate:
`HarchWeb.Markup.Implementation qualified as Impl` cannot be imported from `Lowering.hs` as written,
because `Implementation` already imports `HarchWeb.Markup.Quasi (harch)` (re-exporting it purely so
`HarchWeb.Markup.Syntax` — the module `HarchWeb.Markup`'s facade actually re-exports the quasiquoter
from — can get it from one place), and `Quasi` imports `Lowering`. Adding `Lowering → Implementation`
on top of that closes a genuine cycle:
`Implementation → Quasi → Lowering → Implementation`. Per this document's missing-capability
protocol, the options were: add the primitive despite the obstacle (impossible — GHC will not
compile a module import cycle, TH or not), work around it in the application layer (not applicable
— this is a framework-internal structural problem, not something callers can route around), or flag
and stop. A fourth option existed here that the protocol's three-way fork doesn't name explicitly
but is really a variant of "add the primitive": restructure the *dependency*, not the fix. Since
`Implementation` doesn't use `harch` itself — it only re-exports it for `Syntax`'s convenience —
retargeting `Syntax`'s import to `HarchWeb.Markup.Quasi (harch)` directly and dropping the
re-export from `Implementation` removes the only edge causing the cycle, with no behavior change to
any public facade (`HarchWeb.Markup`'s own exports are unaffected — `Syntax` still exports `harch`,
just sourced one hop closer to its actual definition).

Two smaller decisions followed the same shape as the module's own analysis. First, component names
(`lowerComponentNode`, `reifyComponentProps`'s `lookupValueName`) are deliberately **not** resolved
through quoted `Name`s — they are the framework's intended open extension point, meant to resolve
against whatever the splice site has in scope, and treating them as unhygienic would misread the
bug: the finding is specifically about identifiers meant to be *fixed and framework-owned* being
resolved as if they were as open as a component reference. Second, `nativeTagConstructor`'s
`fromMaybe ""` sentinel (also named in the task's own "after" sketch) was closed by making
`Parser.hs`'s `TagKind`'s native branch carry a `Maybe String` rather than by giving `Parser.hs` a
new dependency on TH `Name`s: `Parser.hs` stays a plain, TH-free recognizer of a closed tag
vocabulary, and *all* `String → Name` resolution stays centralized in `Lowering.hs`'s own tables —
extending the existing division of labor between the two modules rather than blurring it.

The hygiene fix surfaced concrete, previously-load-bearing evidence rather than a byproduct: with
`mkName` gone, six `examples/two-pages` modules that had explicitly imported framework identifiers
(`text`, `element`, `fragment`, `dataAttribute`, …) unqualified — for no reason the application code
itself needed — lost every one of those imports to `-Wunused-imports`, confirming those imports
existed *only* to satisfy the old unhygienic splice resolution, exactly the coupling this fix was
meant to remove.

### Follow-up decision — BV: catch the crash instead of chasing full TH-quote support (2026-08-21)

**Decision: convert `haskell-src-meta`'s uncaught crash on Template Haskell name-quote syntax into
a clean parse failure, rather than implementing actual support for that syntax.** This is the
missing-framework-capability protocol's third fork in a form worth naming precisely: the capability
gap here is not in this codebase's own framework at all, but in a third-party dependency
(`haskell-src-meta-0.8.16`'s `toExp` has no case for `VarQuote`/name-quote AST nodes and calls
`error` instead). Confirmed directly (`cabal repl`, forcing `Meta.parseExp "'Just"`), not assumed
from the task's own note, since that note's "distinct lowering-capability decision" framing
undersold the actual severity: the failure is not a graceful `Left` the existing `case` already
handles, it is an *uncaught* `ErrorCall` that would surface as a confusing library-internal panic at
whichever application module happens to compile the offending markup literal — far from where the
real mistake is.

Genuinely adding TH-quote support means either waiting on an upstream fix to a dependency this
project doesn't control, or hand-writing quote-syntax parsing to bypass `haskell-src-meta` for this
one construct — real, open-ended scope for a syntactic form no call site in this codebase, in any
example app, has ever needed. That tips this toward "flag and stop" for the *feature*, while still
leaving a small, clearly-bounded, unambiguously-beneficial fix available for the *crash*: force
`Meta.parseExp`'s result inside `Q` (`runIO` wrapping `try (evaluate expression)`) and reroute a
caught `ErrorCall` through the same `failAt` path an ordinary parse error already takes. This is
deliberately scoped to the *direct* case — a name quote written as the entire `{...}` expression,
matching the finding's own example — since WHNF forcing does not reach a quote buried inside a
larger expression tree (`{f 'Just}`); reaching that would need full-AST forcing (an `NFData`
instance and `deepseq`, or a hand-written traversal) for a case with, again, no demonstrated need.
Recorded as a named, narrower residual gap rather than silently treated as closed by the WHNF fix.

### Follow-up decision — BX: explicit-prop caching, not a second global CAF (2026-08-21)

**Decision: cache the Gmail access token behind an explicitly-owned `GoogleWorkspaceTokenCache`
prop, and pass the Gmail HTTP manager explicitly, rather than reaching for `unsafePerformIO`/
`NOINLINE` globals the way `Otlp.hs`'s existing manager does.** BX's own text points at
`Otlp.hs:103` as the correct precedent to copy ("`Otlp.hs:103` in the same package does it
correctly with a `NOINLINE` global"), but by the time this task was reached, BZ (later in the same
file) had already named that exact pattern — `Otlp.hs`'s global manager and span counter,
`Acme/Challenge.hs`'s global challenge-directory `MVar` — as a violation of this document's
explicit-props rule ("the `NOINLINE` pragmas are all present and correct — the objection is the
ambient ownership, not the CAF mechanics"). Copying BX's own suggested fix verbatim would have
added a *third* instance of a pattern this project already has an open task to remove, the turn
right after finding it. Instead, both the new token cache and the HTTP manager are threaded as
ordinary explicit parameters: `newGoogleWorkspaceTokenCache :: IO GoogleWorkspaceTokenCache`
allocates the `MVar` once, for whoever constructs the provider to own and pass in, and
`runGmailHttpRequest` now takes a `Manager` argument instead of minting one per call. A caller that
wants two independently-refreshing providers, or a test that wants isolated state per case, simply
allocates two — the same flexibility a global CAF forecloses.

Investigating before implementing also surfaced that neither `HarchWeb.GoogleWorkspace` nor
`HarchWeb.Gmail` has a real caller: `web-api`'s composition root wires email delivery through plain
SMTP only (`WebApi/App.hs`'s `runtimeEmailDelivery`), and neither module is even re-exported through
the `HarchWeb` facade — confirmed by grep, not assumed. Unlike DG's ACME native protocol client
(where commit history documented an explicit prior removal-and-replacement, making deletion the
right call), there is no such evidence here that this subsystem was tried and abandoned; it reads
as a partially-built alternate email-delivery path not yet wired to a configuration choice, not dead
code from a superseded design. Absent that distinguishing evidence, this fix was scoped and shipped
as requested rather than unilaterally deleting a subsystem with real tests and no signal it is
meant to go away — but the "no live caller" fact is recorded here so a future task touching this
area doesn't have to rediscover it.

### Follow-up decision — BY: real ASN.1 decoding, hand-adapted structural matching (2026-08-21)

**Decision: replace the hand-rolled byte-level DER walk with `asn1-encoding`'s real decoder, but
hand-write the RSA/PKCS#8 structural pattern-match against its `[ASN1]` output rather than reusing
`crypton-x509`'s ready-made `PrivKey`/`fromASN1` decoder.** BY's own text named `crypton-x509` as
available and already used by the test suite, which reads as an implicit recommendation to reuse
its private-key decoder wholesale — the natural extend-vs-new-abstraction choice would have been to
do exactly that. Trying it first (rather than assuming the hand-adapted route was necessary) surfaced
a real blocker: `crypton-x509`'s `PrivKey` wraps the `crypton` package's `RSA.PrivateKey`, and this
project's own RSA usage throughout `GoogleWorkspace.hs` (`RSA.sign`, `RSA.PublicKey`, `signRs256`) is
built on `cryptonite`'s `RSA.PrivateKey` — a same-named, structurally-identical, but *nominally
distinct* type from a different package, confirmed by a genuine `Couldn't match expected type`
compile error, not assumed from documentation. This is the missing-framework-capability protocol's
"the workaround silently substitutes a materially different property" case: reusing `crypton-x509`
verbatim would have meant migrating this module (or the whole package) from `cryptonite` to
`crypton`, a materially larger, differently-scoped change than "replace a DER parser." Given that,
the fix keeps `asn1-encoding`'s real decoder (the part that was actually buggy — indefinite-length
acceptance, `Int` overflow, unsigned-integer folding) but writes the RSA/PKCS#8 structural match by
hand against its `[ASN1]` token output, mirroring `crypton-x509`'s own `rsaFromASN1` pattern shape
(read directly from its source as a reference) adapted to construct this project's own `cryptonite`
`RSA.PrivateKey` instead.

A second, unplanned finding surfaced while verifying the fix against every one of the original
task's malformed-input test cases rather than assuming the replacement was correct by construction:
`asn1-encoding` itself is not exception-safe against every malformed input. A zero-length DER `BIT
STRING` — invalid per the DER spec, since its content must begin with an "unused bits" count byte —
crashes the decoder with an uncaught `Data.ByteString.head: empty ByteString` partial-function error
instead of returning a `Left`, discovered because the test asserting on that specific malformed input
failed with a raw exception message rather than a clean domain error. `eitherToIoError` (the shared
`Either Text value -> IO value` boundary every rejection in this module already passes through) now
forces its argument inside `IO` and catches any exception this decoding chain might raise, converting
it into the same "Google Workspace ..." domain error every other rejection already surfaces — closing
the actual security property BY cared about (malformed key material must fail cleanly, not succeed
silently or crash unexpectedly) more completely than reusing `crypton-x509` alone would have, since
that library's own decoder shares the same underlying `asn1-encoding` crash risk.

### Follow-up decision — BZ: two explicit-props fixes, and a mid-task correction to how coverage gaps get closed (2026-08-21)

**Decision: `HarchWeb.Acme.Challenge`'s certbot webroot list becomes a `CertbotWebrootStore` prop
matching `AcmeChallengeStore`'s already-correct shape; `HarchWeb.Observability.Otlp`'s HTTP manager
becomes a plain allocator with no framework-owned global at all, and moves to `web-api`'s own
`App.hs` as the real, single caller — not a second framework CAF.** The two findings in BZ's own
text needed different treatment once actually plumbed through. The webroot store had a direct
sibling already doing this correctly one function up in the same module, so extending that exact
shape was immediate. The OTLP manager did not: the missing-framework-capability protocol's
"add the primitive to the framework" fork does not fit a resource with exactly one real caller in
this tree — `web-api`'s `App.hs` already carries an identically-justified global (`otlpExportQueue`,
recorded under AU) for the same reason, so the manager joined it there rather than becoming a
second, framework-owned CAF nothing else uses. The stale "same idiom `HarchWeb.Observability.Otlp`
already uses" line in AU's own comment was corrected in the same change, since it would otherwise
have been wrong the moment this landed.

**A second, more consequential decision happened partway through this task, prompted directly by a
question about it, not discovered independently:** two coverage gaps this refactor surfaced were
initially closed the way this codebase's memory documents doing dozens of times this session —
`$!`-forcing plus `{-# ANN ... "HLint: ignore Redundant $!" #-}` — which is exactly the pattern task
CB already names as banned by AGENTS.md's own existing rule. That rule had been treated as
narrower in practice than its text: applied when adding *new* strictness to fake a metric, not
recognized as covering this project's own established CSE-workaround technique too. The correction
was twofold: first, elevate the rule out of one AGENTS.md sentence and one task's prose into a
standalone, linked section here (**Never mask a gate finding with an ignore pragma**, in
"Design decisions before you build") so it is load-bearing before the next task reaches for the
same shortcut, not just documented after the fact. Second, apply it retroactively to BZ's own two
new instances rather than leaving them as an exception: one was replaced outright with a genuine
fix (a direct unit test for register/unregister, requiring `CertbotWebrootStore`'s constructor to
be exported the way `AcmeChallengeStore`'s already is — the actual missing piece, not a metric
workaround). The other was *verified*, not assumed, to be a genuine last resort: removing it and
re-running the full coverage gate reproduced the gap on that exact expression, and reading why
confirmed it is not the literal-duplication case the new rule's own worked example fixes by
deduplication — there is nothing duplicated to name once, only two real, both-necessary references
to one already-correctly-factored `let` binding, where GHC's sharing of that thunk credits only the
first reference. Keeping the `$!` there, with a comment stating exactly what was tried and why
deduplication does not apply, is what the new rule itself asks for as its last-resort case — not a
quiet exception to it.

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
| PostgreSQL and custom adapters | Implemented (partial — see AY) | Keep operations typed and interpreters app-selectable. Runtime queries now share a bounded `WebApi.Postgres.Pool` instead of one connection per query (2026-08-21). `sslmode` still defaults to `prefer` (deferred to a deployment decision, not this codebase's to make unilaterally) and migrations still run one `psql` subprocess per statement with no transaction or advisory lock (tracked by AX). |
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
