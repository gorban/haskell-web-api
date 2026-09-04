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

**Module-graph boundary (FQ3, 2026-08-29): Cabal manifests, not the test preprocessor, own each
test suite's complete home-module graph.** Cabal configures a component before `hspec-discover` and
the `SPEC` processor compile generated inputs, so a preprocessor cannot provide truthful
`other-modules` metadata at the point Cabal needs it. Each affected test suite lists every
checked-in test and test-support module in `other-modules`, while normal generated application
modules remain in Cabal's `autogen-modules`. The manifest gate derives the expected names from the
declared source roots and rejects either a missing or a stale entry. This keeps the processor's
single responsibility (source transformation) intact and lets the compiler report a genuine
home-module omission instead of suppressing it.

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

### Decision record — native main element and language attribute (MX, 2026-08-27)

**Decision: extend the existing closed `Html` AST and `harch` quasiquoter with `mainTag` and
`lang`.** The localization example needs a semantic document-main landmark and a dynamic language
attribute. Both are standard HTML vocabulary, general to every application, and belong to the
framework's existing typed markup boundary; adding an application-only raw-HTML fallback or a
general arbitrary-attribute escape hatch would create a parallel, less-safe authoring path. The
new primitive continues to use the central renderer, so dynamic text and language values are
escaped exactly as for every existing typed attribute.

### Decision record — pre-render page-security response capability (AHI-4C, 2026-09-03)

**Decision: extend the existing response interpreter with a closed
`NonPageResponse` subset for protocol route handlers and endpoint guards;
reserve page materialization to `Site`.** The earlier shared `Response` sum
allowed a post-match guard or a protocol route to return a `PageResponse` even
though it had not received the request's pre-render `PageSecurity`. That made
the required nonce/CSRF ownership a convention rather than a type-level
property. A second page/security dispatcher would duplicate routing and guard
order, while page-only guard continuations would add an unnecessary third
response path.

`NonPageResponse` contains the existing body, redirect, action, SSE, and raw
protocol forms and is interpreted by the same final response renderer. A page
handler returns only `PageResult`; after it has received framework-prepared
security, `Site` is the one place that combines the two into a full page
response. This preserves redirects for authentication challenges and all
existing protocol behavior, but prevents APIs, assets, SSE handlers, and
guards from manufacturing a page outside the page-security lifecycle. It is
an extension of `Response`'s existing renderer, not a parallel dispatcher.

### Decision record — typed redirect with grant headers (AHI-4C, 2026-09-04)

**Decision: extend the existing typed internal-redirect response with an
optional header-bearing form, while retaining exclusive renderer ownership of
`Location`.** A native form fallback must establish a newly issued session and
clear the preceding CSRF cookie before it performs its typed 303 navigation.
The old internal redirect could carry only a `ResponseBody`, leaving an
application to choose between an untyped raw protocol redirect or a redirect
which silently lost its session cookie. Neither preserves the single routing
authority promised by the action/native equivalence contract.

`NonPageInternalRedirectResponseWithHeaders` and its corresponding final
`Response` form carry ordinary response headers to the existing renderer. The
renderer discards any supplied `Location` header and constructs the sole
location from the typed `RouteRequest`; headers such as `Set-Cookie` survive.
This is a small extension of the established response algebra rather than a
new native-form dispatcher or a raw-URL escape hatch. Unit coverage proves a
supplied attacker-controlled location cannot replace the typed destination.

### Decision record — pluggable signed CSRF authority (AHI-4C, 2026-09-03)

**Decision: add one `CsrfProtection` capability at the existing page/action
lifecycle; provide an immutable-key-ring signed implementation, while leaving
durable synchronizer storage application-owned.** Action execution already
owns bounded form intake, exact-one host-cookie/submission parsing, and the
boundary before a handler begins. A CSRF middleware or app-local dispatcher
would duplicate that ownership and make a backend responsible for transport
properties it cannot reliably observe. Harch therefore performs the mandatory
double-submit transport check once, and the selected backend only issues a
page token or verifies it against current application binding state.

The default implementation signs a self-contained, bounded token with a
dedicated HMAC key ring, issuance time, expiry, random nonce, and digest of
canonical application grant state. Key IDs are bounded before lookup, signing
keys are redacted, and the configured active/verification set is immutable for
the process lifetime so deployment-driven rotation has a reviewable overlap.
Harch neither knows account/session storage nor accepts raw storage handles:
an application resolves its grants to an opaque binding on both issuance and
verification. A PostgreSQL synchronizer implementation can use the same
capability and report unavailable distinctly, without teaching Harch about
PostgreSQL or creating another CSRF mode in the request dispatcher.

`SimpleSiteConfiguration` therefore requires a `CsrfProtection`; it no longer
inherits an accepting demonstration verifier. This makes a page-capable site
name its authority at construction, including when it selects the supplied
signed implementation. `apiOnlySite` is the deliberate exception because it
cannot declare pages; its internal capability reports unavailable if an
impossible action/page path reaches it. The runnable `two-pages` executable
generates one process-local development signing key and injects it explicitly;
it compiles no secret. Production applications instead provide their immutable
deployment key ring and rotation overlap. This is an extension of the existing
site composition record, not an implicit global key source.

`CsrfToken` and its generation, parsing, redaction, and constant-work
comparison live in `HarchWeb.Csrf`, not in `HarchWeb.Session`. This prevents a
session record from becoming an accidental CSRF authority and lets a native
fallback receive the opaque value from its page's pre-rendered `PageSecurity`.
The fallback still uses Harch's exact-one host-cookie/hidden-field transport
validation, then asks the same selected `CsrfProtection` to verify current
binding state. It therefore cannot introduce an app-specific cookie, an
accepting equality check, or a backend-verification bypass.

### Decision record — typed account-JWT cookie policy (AHI-4C, 2026-09-03)

**Decision: extend `HarchWeb.Authentication` with a validated host-only JWT
cookie policy and opaque renderer.** Harch already owns bounded extraction of
the configured JWT cookie, but without the matching issuance/clear operation
every application would rebuild an authorization-bearing `Set-Cookie` string.
That is a small, general gap in an existing authentication boundary, so an
application-local helper would duplicate security ownership and could drift
from the extractor's configured name.

The policy requires an `__Host-` name and a positive issuance lifetime and
always renders `Path=/`, `Secure`, `HttpOnly`, and `SameSite=Strict`, with no
`Domain` attribute. Clearing is a separate no-token operation with `Max-Age=0`
so logout cannot reflect an untrusted credential. `EncodedJwt` can also model
untrusted input for verification tests, therefore the renderer checks that its
opaque bytes are UTF-8 cookie octets rather than treating its constructor as an
issuance proof. Harch still neither loads keys nor makes claims: `web-api`
selects its JWK material at startup and supplies the issued compact JWT.

### Decision record — application-owned JWT claims and durable principal (AHI-4C, 2026-09-04)

**Decision: keep compact-JWT verification generic in `HarchWeb.Authentication`,
but establish the `web-api` account principal only after an application-owned
durable-session lookup.** The framework is the right owner for bounded cookie
extraction, allowed-algorithm verification, typed authentication failures, and
the guard lifecycle. It cannot know whether an application maps a subject to
an account, a service identity, an external tenant, or a revocable durable
session. A framework account store would therefore duplicate application
identity policy, while parsing the raw cookie directly into `AppRequestContext`
would make an unverified bearer credential appear to be a grant.

At startup, `web-api` reads a configured RS256 private JWK and verification
JWK set, checks the active key ID and RSA key shapes, and fails before binding
a listener if either configuration is invalid or unavailable. It issues only
issuer, audience, account subject, session ID/JTI, and standard times. The
generic verifier validates issuer, audience, algorithm, signature, and time;
the application then loads the referenced session, checks expiry and account
identity, and creates the opaque `AccountPrincipal`. Login renders the typed
host-only cookie and logout revokes that durable session before clearing it,
so a still-unexpired signed token cannot survive revocation. Page/action
workflows consume the established principal and do not reread a raw cookie or
session as an alternate authority.

This deliberately stops short of bearer-token/API conflict policy and
multi-profile OAuth semantics. AHI-4D owns those authentication profiles and
must extend this one guard boundary rather than introduce a `web-api`-local
extractor.

### Decision record — declared action-owner route for pre-decode guards (AHI-4C, 2026-09-04)

**Decision: extend the existing `ActionCodec` and `ApplicationModule`
composition boundary with a typed action-target-to-owning-route mapping, rather
than infer ownership from an action URL or introduce an action router.** Client
actions already have one validated declaration owner: `ActionCodec` matches
their captured method/path and exposes its closed action target before body
decoding. A page `RouteCodec` intentionally need not match `/actions/...`, so
using its ordinary 404 result for pre-decode guards made route-family policy
accidental and could not support a typed guard navigation safely. An
application-local path-prefix check would duplicate routing and fail for a
pluggable application with dynamic paths or non-page actions.

Each module now declares `moduleActionRoute` for its own validated target.
Mounts carry the corresponding target prism, sibling composition selects the
one declared owner, and locale adaptation adds its trusted locale only after
the action codec has matched. The server uses that route solely for endpoint
metadata observation and guards before decoding the body; action dispatch,
CSRF transport, and response rendering remain the established shared
interpreters. A declared action whose path has no ordinary page route therefore
still receives the correct authentication/admission policy, while an unknown
action receives no invented owner. This is a framework capability completed by
the current AHI-4C slice, not completion of AHI-4C itself. The application
now supplies the durable admission credential/TOTP workflow, keyed PostgreSQL
budgets, synchronizer CSRF adapter, cleanup, and enhanced/native action paths
through this capability. Runnable deployment provisioning and the remaining
browser matrix still require task-level completion evidence.

### Decision record — generic authentication-attempt reservation lifecycle (AHI-4C, 2026-09-04)

**Decision: extend `HarchWeb.Authentication` with the storage-neutral,
cancellation-safe reservation hand-off, while retaining typed budgets and
durable stores in each application.** `web-api` already had the correct
protocol for a grouped login attempt: reserve before interruptible proof work,
settle a known result, cancel an indeterminate result, and cancel again when
settlement fails. AHI-4C needs the same protocol for admission credentials and
TOTP. Copying it would make exceptional-path semantics diverge; moving the
account store, account-specific scopes, or PostgreSQL behavior into Harch
would instead make a framework authentication policy out of application data.

`AttemptReservationStore` is therefore a small typed capability supplied by
the application. It admits one atomic, application-defined reservation and
offers settle/cancel operations; its budget, reservation, lockout, and error
types remain opaque to Harch. The generic runner maps ordinary throttling and
store failures through application constructors, keeps proof and settlement
interruptible under a narrow masked hand-off, and leaves crash recovery to the
application store's retention policy. `web-api` delegates its established
login lifecycle through this boundary, and the composed admission adapter uses
the same lifecycle with its own namespace and PostgreSQL store. This only
completes shared lifecycle ownership: application-specific provisioning,
deployment wiring, and complete browser proof remain AHI-4C work.

### Decision record — composed synchronizer CSRF adapter (AHI-4C, 2026-09-04)

**Decision: implement the durable synchronizer form as a composed-application
adapter to `CsrfProtection`, not as a second framework CSRF mode.** The shared
Harch boundary already owns exact-one host-cookie/submission extraction and
constant-work equality before a backend runs. Recreating those steps in a
PostgreSQL handler would make the native and enhanced paths drift, while
teaching Harch a database table would reverse the intended storage-neutral
ownership.

The composed adapter hashes each generated opaque token before passing it to
its supplied store, along with Harch's opaque current grant binding and the
earliest permitted expiry. Harch exposes only the binding's domain-separated
SHA-256 digest as base64url for that store contract; raw principal and session
values never cross the boundary. It prunes deterministically before bounded
issuance, reads durable state on every verification for immediate revocation,
and maps store failure or capacity exhaustion to the existing unavailable
rail. Its only small framework extension is a positive opaque cookie-lifetime
constructor, so an application backend cannot invent an invalid transport
lifetime. The composed migration ledger owns the three separate tables and the
adapter's parameterized cleanup, bounded per-binding issuance, and
current-state verification queries. The remaining task-level evidence is
runnable deployment pool configuration and the complete browser coverage, not
a framework-owned PostgreSQL mode.

### Decision record — opaque-session cookie extraction (AHI-4C, 2026-09-04)

**Decision: extend `HarchWeb.Session` with an exact-one parser for a configured
opaque-session cookie, rather than duplicate Cookie-header handling in the
admission application.** A durable admission session is application-owned,
but matching its configured cookie name, rejecting a malformed or duplicate
value, and enforcing the opaque session-ID grammar are transport concerns that
must be the same for every application selecting that session form. A generic
arbitrary-cookie accessor would encourage applications to turn unchecked text
into a credential; selecting the established `SessionCookieName` instead
retains the bearer-token type at the boundary.

The parser sees only bounded request headers after the request-head policy,
returns missing, malformed, ambiguous, or an opaque ID, and redacts a found
ID in diagnostics. It never looks up a session, chooses first-cookie
precedence, or decides access. `composed-domains` uses it for its distinct
host-only admission cookie, while retaining its own session lifetime,
PostgreSQL store, revocation, and public challenge policy.

### Decision record — separate composed admission policy axis (AHI-4C, 2026-09-04)

**Decision: model admission as a closed application policy installed in
`beforeAuthenticationGuards`, with its principal held in the root context's
separate local axis.** Reusing account identity would let a TOTP admission
grant look like an account scope or MFA result. A second authentication
pipeline would duplicate the framework guard lifecycle. The composed root
instead classifies its own route algebra exhaustively: the admission page and
public assets continue without admission, while login and mounted Catalog and
Orders routes require it. An enabled policy cannot assemble against an
`AuthenticationDisabled` root, so the intended admission-then-account shape
cannot silently become an anonymously accessible domain application.

The implemented example establishes an active durable admission-session
principal from an exact-one host-only cookie and returns typed internal
challenges (or 503 for an unavailable store). Its encrypted credential
provisioning, TOTP action/native fallback, keyed attempt budgeting, durable
PostgreSQL store/migrations, synchronizer CSRF backend, and cleanup remain
application-owned. AHI-4C is nevertheless not complete until runnable
deployment provisioning and the full browser proof matrix are green.

### Decision record — typed action navigation at the existing response boundary (AHI-4C, 2026-09-03)

**Decision: extend `ClientActionResponse` with `ActionNavigation route context`
and render it only through the root `RouteCodec`.** A raw URL field or
application-supplied URL callback would create a second routing authority and
would make it too easy for a mounted module to emit an unscoped or external
destination. The closed `StayOnCurrentRoute | NavigateInternal HistoryMode
(RouteRequest route context)` algebra keeps an action's destination subject to
the application's established route rendering and same-origin `SafeUrl`
boundary. External redirects remain a distinct capability rather than an
overloaded text value.

Composition adapts the navigation destination with the same route/context
projection that it already uses for pages and non-page protocol results. The
browser action runtime applies patches, settles the capture-kernel lifecycle,
and only then invokes the existing navigation runtime; when a destination is
present it leaves focus and announcement to that document navigation lifecycle.
This is an extension of the existing typed response/rendering pipeline, not a
parallel action router. Native fallback uses the paired
`NonPageInternalRedirectResponse`: its protocol handler verifies the same
bounded form and CSRF transport, then returns a `303` whose `Location` is
rendered by that same root codec. The two-pages subscription example proves
the enhanced `PushHistory` and native `303` variants converge on one typed
destination. Retained-action reauthentication and the remaining full browser
evidence are AHI-4C work. `web-api` already applies
`ReplaceHistory` after it has durably created the account session and returned
the session-cookie plus CSRF-clear transition; its destination is the typed
profile route, so browser Back does not reopen the credential-bearing form.

### Decision record — bounded retained action reauthentication (AHI-4C, 2026-09-03)

**Decision: extend the existing capture kernel and typed client-action
response path, while leaving the reauthentication modal and login workflow
application-owned.** The capture kernel already owns the one bounded form
envelope and its lifecycle, so copying fields into an application event, web
storage, or a second retry queue would create a parallel and less safe action
authority. A framework-owned action-authentication challenge is the sole
response marker the deferred action runtime recognizes. On that marker alone,
the kernel retains its existing envelope in tab memory for a positive,
per-control configured lifetime (ten minutes by default), emits only an opaque
action ID to application JavaScript, and exposes a deliberate one-time replay
operation. It discards the envelope on cancellation, navigation, expiry, and a
second challenge; regular 4xx/5xx results do not enter this flow.

An application listens for the opaque event, renders its own accessible modal,
uses its ordinary login action, refreshes its current page security, and calls
the replay operation only after user confirmation. Harch does not embed a
credential UI, interpret application login results, persist an action, or
store its fields in `localStorage`, `sessionStorage`, telemetry, or a server
retry table. This extends the existing action/capture ownership boundary rather
than adding a modal dispatcher or an application-specific mutation retry API.

### Decision record — PostgreSQL database-change ledger (AHI-4C, 2026-09-03)

**Decision: extract the existing connection-scoped migration transaction into
the PostgreSQL-specific `postgres-database-changes` package, with each
application supplying an immutable ordered `DatabaseChange` plan and its own
ledger location.** The previous `WebApi.Postgres.Migration` protocol already
owned one privileged connection, one transaction, an advisory lock, and
rollback. Keeping that machinery in one reference application would force
`composed-domains` either to import WebApi configuration and migrations or to
reimplement a security-sensitive ledger. It is not a Harch capability:
PostgreSQL connections, SQL, role management, and schemas remain adapter and
application concerns.

The new package therefore extends the existing PostgreSQL adapter boundary
rather than adding a framework migration API. It validates application-owned
ledger identifiers before constructing SQL, performs the legacy
`schema_migrations` cutover in the same transaction, and stores a SHA-256 of a
length-delimited UTF-8 statement sequence together with the stable change ID
and position. A history is only a contiguous prefix of the supplied plan: an
unknown, altered, missing, or out-of-order record fails startup and rolls back.
Deployment role/password reconciliation remains an explicit application-owned
final SQL sequence in that transaction, not mutable desired-state application
data and not an invented database change.

### Decision record — configuration diagnostics and Certbot credential policy (PR-SEC5, 2026-08-28)

**Decision: preserve the existing configuration and startup-plan boundaries, but make their
diagnostic rendering non-disclosing and retire the arbitrary runtime Certbot-argument setting.**
OTLP headers are explicitly designed to carry exporter credentials, so a generic logging wrapper
would be a second, weaker observability boundary; the existing exporter/configuration types own
the data and redact it in their `Show` instances. Malformed header configuration reports the
environment key and one-based entry position, never the whole value. Derived application startup
types remain safe because they render those same nested values.

Certbot's built-in HTTP-01 flow has all of its non-secret inputs represented in `AcmeConfig`.
Accepting an unconstrained comma-delimited argv override cannot reliably distinguish a custom DNS
credential from an ordinary option, and command arguments can be exposed in process inspection or
persisted by Certbot renewal state. `LISTENER_<n>_ACME_CERTBOT_ARGUMENTS` is therefore rejected
without rendering its value. Literal secret arguments are unsupported. Operators needing DNS-01
or another custom authenticator use the existing executable-path override to run their own wrapper;
that wrapper owns protected credential files or its managed environment and its lifecycle/security
policy. This removes the framework from credential transport rather than inventing a partial
secret-argv filter.

### Decision record — bounded SMTP response accumulation (PR-SEC6, 2026-08-28)

**Decision: extend `HarchWeb.Email`'s existing SMTP response reader with fixed whole-response
limits, rather than expose a new SMTP configuration surface or add a second parser.** The response
reader is the sole owner of provider-controlled reply lines and already owns the per-line bound.
It now permits at most 100 lines and 64 KiB of complete wire lines (including status/separator and
CRLF), which is comfortably above normal greetings and capability replies but bounds both retained
memory and continuation-loop work before authentication. The accounting checks the next line
against the remaining byte budget before retaining it, avoiding overflow or an intermediate
over-limit allocation. Stable errors carry no provider payload. The existing 16 KiB line ceiling
remains defence in depth. Real loopback coverage exercises count overflow in a greeting and after
STARTTLS, plus byte overflow in the pre-TLS EHLO path.

### Decision record — typed forwarded path prefixes (PR-SEC7, 2026-08-28)

**Decision: extend the existing opaque `HarchWeb.PathPrefix` role into the sole validated
forwarded-prefix representation, and construct it once at `HarchWeb.Security`'s trusted-header
boundary.** A second URL-sanitizing layer at each redirect, route, navigation, or static-asset
sink would be incomplete by construction. `PathPrefix` now accepts only canonical absolute paths
made from nonempty URI-unreserved ASCII segments; repeated slashes, dot segments, percent escapes,
backslashes, query/fragment delimiters, controls, and other ambiguous characters fail closed to the
root mount. `RequestPolicyConfig` owns the trust decision, and the parsed value flows through
framework routing, redirects, ACME/static paths, the site runtime descriptor, and web-api request
context rather than re-normalizing raw header text. This preserves ordinary `/app` mounting while a
proxy that appends a client-controlled header can no longer turn generated `Location`, `href`, or
`src` values into external browser references.

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
entropy source. The server passes that nonce only to document rendering and CSP construction.  Client
action CSRF state is a separate opaque token: the framework renders it in a `__Host-harch-csrf` secure,
same-site cookie and validates its typed double-submit transport, while the application supplies the
per-page token and authorizes session-bound actions through its existing session store (CZ,
2026-08-23). Non-page responses carry neither value. `renderDocumentForTests` is intentionally deterministic
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

The web-api reference application records the AHI-1 ownership decision: repeated page and profile
markup lives in private `WebApi.Components.*` modules as pure typed functions with explicit prop
records, while its theme and responsive layout remain in `public/styles/app.css`. Its app shell
declares that stylesheet through `Stylesheet` and applies the request context's already-validated
`PathPrefix`. This extends the existing shell/static-asset boundary; it is not evidence for a Harch
design system, a CSS EDSL, or another forwarded-prefix parser.

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

Server dispatch gives a page or API route's synthesized @HEAD@ and @OPTIONS@ result precedence over
the client-action header, so a capability response cannot run a state-changing action. Ordinary
client-action endpoints are a separate declared protocol table and may not appear in the page route
table; their codec then owns the declared action method, path, and decoder. For a route endpoint, it
first matches the rendered path and declared method, then runs only that endpoint's
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
independent `MissingApiField`/`DuplicateApiField`/`InvalidApiField` rather than stopping at the first one.
Its opaque public representation returns a decoded value, a non-empty ordered parse-error collection,
or an explicit invalid-codec outcome; the latter is a generic endpoint rejection, so a field-failure
renderer cannot receive an empty rejection. This matches
`HarchWeb.Action`'s decoder shape; `apiRequestDataFromWaiRequest` extracts the `ApiRequestData`
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
`apiEndpointFamily` validates one non-empty, duplicate-free table before
`apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition` derive one
`HarchWeb.RouteCodec`/`RouteDefinition` route family from it (see the closed route-family registry below) — the
shared server dispatcher supplies the real WAI request only after it has selected the route and enforced
the table's method policy, so API declarations cannot create competing 404/405/`Allow`/`HEAD`/`OPTIONS`
behaviour with any other route family in the same application. A bounded buffered decoder maps oversized,
unsupported-media, and malformed input to 413, 415, and 400 respectively, and passes expected domain
failures through the endpoint's explicit interpreter. Additional streaming codec shapes and multipart
storage policy are delivered: endpoint declarations select either the bounded buffered, streaming, or
scoped multipart consumer, and multipart storage remains application-selected through its adapter.

**Historical note (2026-08-13):** this typed endpoint boundary previously coexisted with two now-removed
compatibility layers: a legacy `ApiEndpoint`/`apiEndpointMiddleware` target-plus-handler table, and an
intermediate `apiRouteEndpointMiddleware` that derived `404`/`405`/`Allow`/`HEAD`/`OPTIONS` from a typed
`SomeApiRouteEndpoint` table composed as a standalone `Wai.Middleware` rather than through the route-family
registry. Both were deleted once every application in this repository had migrated onto the family
registry; see the AK decision record below for why that made deletion, not relocation, the right call.

### Decision record — FQ1: context-free static action rendering (2026-08-29)

**Decision: extend the established `ActionPath` representation, not add an application-local
wrapper or a second action declaration surface.** Static action declarations now retain their fixed
path alongside the dynamic renderer, allowing `staticActionPath` and `staticActionForm` to require
that proof instead of passing an artificial unit context. Dynamic declarations still require the
existing explicit context APIs, so a static renderer cannot silently use an arbitrary request value.

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
ownership follow AD's completed application-selected adapter policy, so the endpoint does not create
a new upload lifecycle or default to local files.

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

This closes the route-family primitive gap identified above.

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

At the time of this decision, `web-api` still hand-wrote its `/api/status`/`/api/second` dispatch and
the typed endpoint boundary had no way to preserve `/api/second`'s response observability attributes
or private log entry. The following decision added that general `ApiResponse` capability, and the
subsequent AC implementation migrated both endpoints through `apiRouteDefinitionWithContext`; the
current status table records the resulting single-dispatcher composition.

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

### Follow-up decision — EM: bound valid cookie pairs at the existing request-head boundary (2026-08-25)

**Decision: extend `RequestHeadLimits` with opt-in cookie count, name-byte, and value-byte budgets;
do not add another cookie parser or make this an endpoint option.** A raw `Cookie` header can carry
many application-visible pairs, so the existing generic per-header-value limit cannot express the
separate resource ownership AF required. The framework already owns the pre-routing raw-header
gate, while `HarchWeb.Api.Request` owns the later typed cookie projection. Extending the earlier
boundary preserves that ownership sequence: configured cookie limits reject with the established
non-reflective `431` before routing, middleware, observability, or a decoder can create a list of
cookie values; an absent policy retains the previous compatibility behavior.

The gate mirrors the projection's deliberately narrow syntax: it counts and measures only segments
with a non-empty, valid cookie-token name and an `=` separator, across every case-insensitive
`Cookie` header field. Empty values and repeated names remain valid, so downstream typed decoding
still detects duplicate names as it did before; malformed or empty segments remain ignored rather
than turning a resource budget into an accidental cookie-grammar policy change. The scanner walks
raw `ByteString` segments without `split` or a retained pair list. Generic total-header and
per-header-value limits run first and remain independent safeguards. This adds no new default and
does not claim to solve the listener-time limits or per-route policy question retained by EN.

### Follow-up decision — EN: route-local execution admission, not impossible per-route transport policy (2026-08-25)

**Decision: extend `RouteDefinition` and the one shared `toWaiApplication` dispatcher with an
opt-in `RouteExecutionPolicy` whose sole capability is a per-route concurrency gate.** Listener
timeouts, slowloris controls, and wire-header allocation happen before a WAI request or route exists;
the application's request-head validator and global admission gate also run before route selection.
Putting those fields in a route record would promise protection the runtime cannot honor, so no
per-route transport/head override or environment-variable map is added. Endpoint body budgets remain
their already-declared endpoint-consumer capability, not a retroactive request-head policy.

The route gate starts only after the existing middleware has supplied the context used for route and
method selection, and it guards the selected route's remaining response lifetime. It is therefore an
additional limiter, never a replacement for or loosening of the global gate: an application-wide
limit always admits first and still covers middleware; the route gate covers every selected route
outcome (`HEAD`, `OPTIONS`, and method-not-allowed included) until its WAI response completes. The
implementation allocates one gate per bounded route per public WAI adapter, so direct adapters,
local listeners, and runtime listeners share the same behavior without a parallel middleware or
dispatcher. An unmatched path and framework early response have no route policy to select.

Rejection uses the existing non-reflective `503` body and does not add a route name, limit, or
untrusted value to request telemetry or logs; route identities are application ADTs and could be
unbounded in other applications. Applications declare this typed policy alongside the route they own,
while deployment configuration continues to own listener and application-wide budgets. A route policy
may be redundant when wider than a configured global cap, but can never raise that cap because both
gates must admit the request.

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
their real signatures.

Correction (FQ5, 2026-08-29): a signature that begins with `name ::` and places its arrows on
indented continuation lines was still silently undercounted, so the "can never fabricate a
violation" rationale did not justify leaving a mandatory follow-up trigger incomplete. The report
now carries lexical state across the complete top-level signature: line/block comments and strings
are ignored; parentheses, brackets, and braces suppress nested arrows; and ordinary plus
parenthesized-operator names are recognized. A complex untyped equation pattern remains
conservatively uncounted rather than whitespace-tokenized. The fixture covers these boundaries,
and the real-tree comparison exposes the FQ6--FQ9 records instead of treating their prior low
numbers as closure evidence. This keeps the metric an advisory review signal, but makes it a
reliable trigger for the design review AGENTS.md requires.

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

### Follow-up decision — AX: transactional, versioned migrations own one short-lived libpq connection (2026-08-23)

**Decision: extend `WebApi.Postgres.Migration` with a private, owner-credential libpq
interpreter held for one migration batch; do not run migrations through the runtime query pool or
add a second general database abstraction.** The existing migration module already owns schema
evolution, its distinct owner credentials, and the `haskell-web-api-db` lifecycle. AX's original
`psql` command-by-command runner cannot hold a transaction or advisory lock across statements, so
the smallest complete extension is one connection for that module's whole batch. AY's
`PostgresPool` is deliberately a lazy, long-lived, shared runtime-query resource using the
low-privilege runtime identity; using it for a one-shot owner migration would conflate those
lifecycles and keep an otherwise-unneeded idle privileged connection. The migration interpreter
shares only the already-established `runtimeConnectionString` encoding, while owning its own
bracketed connection and transaction cleanup.

The migration representation will be an ordered, versioned collection rather than an inferred
set of `CREATE ... IF NOT EXISTS` statements. Under one transaction it acquires a transaction
advisory lock, creates the schema-version table, reads applied versions, executes each unapplied
migration, records its version, reconciles the configured owner/runtime-role privileges, and
commits; any command failure rolls the entire transaction back. The reconciliation is deliberately
not versioned: role names and passwords are deployment configuration, so recording the initial
schema must not prevent a later password rotation or changed runtime identity from taking effect.
A dedicated test-only migration executor records the same connection-scoped operations so Unit
tests can prove ordering, version skipping, reconciliation, rollback, and error handling without
preserving the obsolete one-`psql`-process-per-statement fixture contract. This is a precise
extension of the existing migration boundary, not a parallel runtime query API.

### Follow-up decision — FQ4: migrations use one typed post-BEGIN failure rail (2026-08-29)

**Decision: express the existing migration transaction as `ExceptT PostgresRunnerError IO`, rather
than manually forwarding `Either` through each migration phase or adding a second transaction
runner.** `WebApi.Postgres.Migration` already owns the sole short-lived privileged connection and
the transaction boundary named by AX. `BEGIN` remains outside the post-BEGIN rail so a failed begin
does not attempt rollback; every later operation, including `COMMIT`, uses the one rail and its
boundary performs best-effort `ROLLBACK` before returning the original typed failure. The executor
adapter lifts into that rail once, so an adapter-level decode failure cannot bypass cleanup.

Applied-version cells remain raw optional libpq bytes until the same typed boundary decodes them.
`NULL` and invalid UTF-8 now return stable `PostgresMigrationFailed` values and trigger rollback;
they no longer rely on the schema's current primary-key invariant or `fromJust`/partial UTF-8
decoding. This is deliberate defence against an old or manually altered table shape. Focused
tests cover both malformed wire values and preserve the existing proofs for every post-BEGIN
failure, including `COMMIT`.

### Decision record — FQ6: one explicit login environment across password and MFA stages (2026-08-29)

**Decision: split the existing `WebApi.Login` implementation by stable lifecycle responsibility,
while retaining its public facade and one authentication protocol.** Password credential lookup,
constant-work verification, and opportunistic rehashing form one stage; MFA proof and reservation
settlement form the next; the existing reservation-admission helper remains their shared lifecycle
owner. A `PasswordLoginEnvironment` therefore groups the credential store, MFA store, throttle,
password-work gate, and injectable rehasher once. `SecondFactorContext` nests that same environment
alongside the per-attempt proof, encryption key, and operation time, so a password and second-factor
attempt cannot accidentally use divergent stores, clocks, or admission policy.

This extends the existing account workflow boundary rather than creating an alternate login service
or an application-local authentication wrapper. `WebApi.Login` stays the consumer-facing facade;
private password, attempt, and MFA collaborators separate the owned stages without widening the
framework surface. The same input-record rule applies to MFA enrollment and verification delivery:
their existing workflow contexts become the public entry points, while response builders derive
locale and request context from the one action request rather than accepting independently
transposable copies. The changes are a deliberate API cleanup, not a metric-only relocation; focused
workflow regressions and the full coverage gate must preserve the present authentication, throttling,
rehash, and session-enrollment behavior.

### Decision record — FQ7: execution records inside the existing endpoint and multipart boundaries (2026-08-29)

**Decision: retain `ApiEndpointContract` and the single route dispatcher as the API boundary, while
giving their private interpreters cohesive execution state.** `ApiEndpointExecution` combines one
already-declared contract with the WAI request and its once-derived request data; buffered, form,
streaming, and multipart decoding therefore cannot be called with separately transposed fields,
body declaration, encoders, failure policy, or request. Route definitions still select their method
from that same contract and invoke the same runtime interpreter. This is an internal wiring record,
not an additional endpoint family, handler protocol, or WAI dispatcher.

Multipart's existing `MultipartConsumer` remains the explicit storage, limits, reader, and callback
environment required by its scoped ownership lifecycle. A private `MultipartDriverState` now keeps
that environment with its scanner, open part, counts, and body-byte total through recursive parser
steps. It prevents accidental mismatches between state components without changing upload storage,
promotion/discard ownership, bounded streaming, or typed parse/callback failures. Focused endpoint
and multipart regressions plus the full coverage gate are the proof that this structural cleanup
preserves their short-circuiting and resource contracts.

### Decision record — FQ8: context records at server ownership boundaries (2026-08-30)

**Decision: group values that remain fixed through one server ownership boundary, but leave each
changing request, route, response, and timing phase explicit.** `RequestObservabilityContext` owns
the application, accepted WAI request, and resolved request policy used by both early and routed
reporting. `RequestExecutionTimingState` owns only timestamps that have already occurred; it becomes
the complete `RequestExecutionTimings` only after the response has been forced. This preserves the
existing `seq` points around policy, route dispatch, response rendering, and observability forcing:
the refactor cannot turn timing into deferred bookkeeping or let pure telemetry exceptions escape at
an unrelated boundary.

`RuntimeRequestEnvironment` and `RuntimeTransportDependencies` likewise own the rendered
application, ACME stores, typed application, and shared listener request limits that one runtime
starts with. Listener-specific endpoint, TLS settings, socket, peer tracker, readiness signal, and
reporter stay explicit or live in a private per-listener record. `SimpleSiteConfiguration` groups a
site's declarative route-table and shell inputs; middleware, policy, actions, and observers remain
ordinary `Site` overrides. The disabled default observers are intentional policy values and were
already reduced by FQ1 to unforced ordinary no-ops, so this task does not disguise them with new
ignore callbacks. Existing request/transport/ACME behavior tests and the coverage gate are the
evidence required for this structural change.

### Decision record — FQ9: injected setup and composition environments (2026-08-30)

**Decision: represent capabilities that are fixed for one execution as cohesive records, while
keeping each command, plan, report, and user-visible result explicit.** Container autostart groups
only the caller's skipped/succeeded/failed result constructors; prerequisite reporting groups its
loader, reachability checks, autostart operations, and destination handle; database setup groups
its two configuration loaders and migration/seed runners; and web-api runtime construction groups
its three coordinated observability reporters. These are all stable dependency bundles that would
otherwise be passed together and can be transposed without a type error. They are not a new
framework abstraction or ambient service layer: the current plan, command arguments, account
workflow, output handle, and response-facing values stay explicit at the operation that varies
them. Focused dependency-injection tests plus the full coverage gate remain the proof that failure
ordering and reporting behavior have not changed.

### Decision record — FQ10: length-delimited ICU UTF-8 boundary (2026-08-30)

**Decision: extend the existing `HarchWeb.Localization` ICU renderer with a length-delimited UTF-8
ABI, rather than reject NUL values or introduce a second formatter.** Locale identifiers, message
templates, argument names, and text arguments are application-owned Unicode values; a C-string ABI
would silently alter those values at the first U+0000. The native boundary now receives an explicit
byte length for every such value and constructs ICU strings from those spans. ICU's output carries its
own length too, so Haskell decodes precisely the returned bytes with `decodeUtf8'`; malformed output
becomes `MessageFormatRejected`, never replacement text or an exception. This keeps the established
pure, deterministic `Localizer` API and its error rail intact while making the FFI contract truthful.

### Decision record — FQ11: client-action protocol interpreter extraction (2026-08-30)

**Decision: extract the internal client-action protocol interpreter from request execution, while
retaining the one shared route/timing/finalization lifecycle.** The interpreter's bounded body read,
origin and double-submit CSRF validation, typed decode, application authorization, and handler
invocation always occur together after a route context is selected. That stable protocol capability
belongs in `HarchWeb.Server.ClientAction.Runtime`; it does not add a dispatcher or a second action
registry. `RequestExecution` continues to own request-head admission, framework early responses,
middleware, route method selection, concurrency admission, monotonic timing, WAI response delivery,
and observability. The public WAI adapter therefore has the same execution order while its module
health falls below the review threshold.

### Decision record — FQ12: account-workflow composition extraction (2026-08-30)

**Decision: extract the private account-workflow construction capability from `WebApi.App`, while
retaining `WebApi.App` as the explicit application/site composition root.** Runtime workflow creation
and the deliberately unavailable fallback both assemble the same stable account-effect record and
must share the one process-wide password-work gate; they belong in `WebApi.App.AccountWorkflow`.
The extracted collaborator receives its database pool and environment explicitly, builds the same
PostgreSQL stores, SMTP policy, clock, verification URL, and fallback stores, and returns the
ordinary `AccountWorkflow` value. `WebApi.App` continues to own site construction, routes, request
policy, page repository, reporters, listener startup, and pool lifetime. This is a cohesive
composition boundary, not an alternate application service or ambient dependency layer.

### Follow-up decision — AY: add `connect_timeout` now, defer a hard TLS default until a deployment decision (2026-08-21)

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
`runtimeConnectionString` builds. The migration-side `WEB_API_MIGRATION_DATABASE_*` parser also
needs the field populated (one shared `DatabaseConfig` record), and AX's later one-shot libpq
migration connection reads it through that same conninfo encoder. It remains a hardcoded committed
default rather than a second migration-only environment variable: a one-shot batch has no
concurrent request thread to starve, and a second knob would add configuration surface without a
separate operational policy behind it. Threading an equivalent timeout into the retained `psql`
seed subprocess environment (`PGCONNECT_TIMEOUT`) was considered and rejected —
`passwordEnvironment` is shared by every psql invocation, and widening it would have broken several
`Integration/WebApiSpec.hs` tests that assert its exact environment against real subprocess runs,
for a code path with no starvation risk to close in the first place.

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

### Follow-up decision — AY: closed libpq transport policy with omitted-value defaults (2026-08-26)

**Decision: extend the existing `DatabaseConfig` and shared connection encoders with a closed libpq
transport-policy ADT; when the operator provides no mode, emit neither `sslmode` nor `sslrootcert`
and preserve libpq's documented defaults.** The configuration accepts only PostgreSQL's six named
`sslmode` values, so `verify-full` is available for deployments that want authenticated TLS and
`require` remains visibly encryption-only. An optional root-certificate path requires an explicit
mode; otherwise libpq uses its own default trust location. The same policy flows to pooled runtime,
migration, and `psql` paths (`PGSSLMODE`/`PGSSLROOTCERT`), rather than creating a parallel client
or a command-path downgrade. This respects application/deployment ownership: the application makes
each available guarantee unambiguous, while the deployment owns which guarantee, CA, hostname, and
server lifecycle are appropriate. The real PostgreSQL 17 fixture now exercises that deployment
contract through libpq: verified success, a valid untrusted CA, hostname mismatch, and a
TLS-disabled listener all have distinct outcomes.

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
leaving a small, clearly-bounded, unambiguously-beneficial fix available for the *crash*: traverse
`Meta.parseExp`'s complete `Exp` inside `Q` by fully consuming its `pprint` representation (`runIO`
wrapping `try (evaluate (length (pprint expression)))`), then reroute a caught `ErrorCall` through
the same `failAt` path an ordinary parse error already takes. `Exp` has no `NFData` instance, and
this existing structural renderer reaches every nested child without adding a second parser or
claiming to implement TH-name-quote syntax. Direct `{'Just}` and nested `{f 'Just}` regressions both
prove the dependency error now becomes the positioned markup failure. Name quotes remain unsupported
syntax, but they no longer leak an unhelpful compiler-time panic.

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

### Follow-up decision — BY/PR-T4: maintained RSA decoding after the crypto migration (2026-08-24)

**Decision: retain `asn1-encoding` for strict DER tokenization and the small PKCS#8-envelope
contract check, then use `crypton-x509`'s maintained `X509.PrivKey` `fromASN1` instance to build the
RSA key rather than reconstructing PKCS#1 fields locally.** BY originally tried that maintained
decoder and correctly stopped when its `crypton` `RSA.PrivateKey` could not satisfy this module's
then-`cryptonite` signing API. ED subsequently replaced the project's direct `cryptonite`
dependencies with `crypton`, making that recorded nominal-type mismatch false. PR-T4 re-ran the
integration against the current build plan and confirmed `X509.PrivKeyRSA` now contains the exact
`crypton` `RSA.PrivateKey` accepted by `RSA.sign`.

The generic decoder deliberately accepts both bare PKCS#1 and PKCS#8, whereas this service-account
boundary promises exactly one PEM `PRIVATE KEY` block containing a DER PKCS#8 RSA key. The module
therefore retains only the outer PKCS#8/RSA OID envelope check and strict DER decoding of its embedded
PKCS#1 bytes before calling `fromASN1`; it no longer hand-matches or constructs the RSA CRT fields.
The outer check is contract preservation, not a parallel parser: a regression proves a bare PKCS#1
payload under a `PRIVATE KEY` label remains rejected. This keeps BY's malformed-input boundary and
its rejection of indefinite/invalid DER while replacing the stale custom key-construction logic with
the maintained implementation.

### Follow-up decision — PR-F2: one checked HTTP field-name boundary (2026-08-24)

**Decision: extract the existing abstract `ApiHeaderName` into the private
`HarchWeb.Api.HeaderName` collaborator, re-export its checked constructor through the public request
API, and require that same type in `ApiResponse`; do not add a response-only validation layer or
leave the existing raw `Text` response field.** Request data already depended on response form types,
so putting the common type in either existing module would create an import cycle. The tiny private
leaf is the narrow shared protocol boundary: it validates a non-empty ASCII RFC 9110 token, stores
the ASCII-lowercase representation for case-insensitive request lookup, and has no knowledge of
request decoding or response rendering.

Public callers now receive `Maybe ApiHeaderName` and must handle runtime-derived invalid names. Only
the private collaborator exposes a literal constructor for fixed framework names such as `Accept` and
`Vary`; this prevents an application from converting request text into a response name by assertion.
The response record itself stores `ApiHeaderName`, so invalid names cannot reach WAI emission after
construction. Regressions cover empty, whitespace, colon, CR/LF/NUL, and non-ASCII rejection, while
valid mixed-case names retain canonical, case-insensitive request behavior.

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

### Follow-up decision — PR-F3: one validated API endpoint family (2026-08-24)

**Decision: add the opaque `ApiEndpointFamily` smart-construction boundary to the existing
route-family adapter, rather than retaining two raw endpoint-list arguments or creating another
dispatcher.** The existing `RouteCodec`/`RouteDefinition` pair is still the sole owner of path and
method policy; the gap was that `apiRouteEndpointFamilyCodec` and
`apiRouteEndpointFamilyDefinition` each independently accepted `[SomeApiRouteEndpoint]`, letting a
caller accidentally supply different tables. The new `apiEndpointFamily` constructor rejects an
empty family and reports the precise duplicate `ApiPath`/`ApiMethod` declaration, while permitting
different methods at the same path. Its private non-empty representation is then the only input both
adapters accept, so public callers cannot construct mismatched dispatch halves or rely on the old
first-declaration-wins behavior. This is a small extension of the existing route-family boundary,
not a replacement registry and not a parallel WAI dispatcher.

The family definition still retains its typed direct-invocation 404/405 guard. That branch is no
longer an attempt to repair a public construction mismatch; it remains the total, safe behavior for
the codec's synthetic not-found route and for a caller that invokes a `RouteDefinition` outside the
normal shared dispatcher. Unit coverage proves empty rejection, precise duplicate rejection,
same-path/different-method acceptance, and normal heterogeneous dispatch.

### Follow-up decision — PR-F4: total typed API request decoding (2026-08-24)

**Decision: make `RequestCodec` an opaque newtype whose runner returns
`ApiRequestDecodeResult`, rather than retaining its public nested `Compose` representation or adding
an endpoint-only invalid-decoder fallback.** `RequestCodec` already owns field declaration,
applicative accumulation, and endpoint input interpretation. Its former transparent shape let an
application construct impossible `([], Nothing)` and `(errors, Just value)` results that the field
combinators never produce; then endpoint runtime had to guess how to interpret those malformed
states and could pass an empty list to a field-failure renderer. Extending this existing boundary
keeps one owner for the invariant: the public outcomes are `ApiRequestDecoded value`,
`ApiRequestRejected (NonEmpty ApiRequestParseError)`, and the explicit invalid-declaration outcome
`ApiRequestCodecInvalid`, with the accumulating `Applicative` preserving
declaration order when both independent fields reject.

No parallel validation pass was added. Applications still compose `requiredField`, `optionalField`,
and `fieldWithDefault` exactly as before; a deliberately invalid declaration is explicit and becomes
a generic 400, never a field-failure callback. Runtime converts the non-empty rejection to a list
exactly at that callback boundary, so the callback's established API stays compatible while the
empty-list case becomes unrepresentable. Focused regressions exercise decoded values, each ordinary
field rejection, ordered accumulation, form decoding, explicit invalid declarations, and
field-failure rendering.

### Follow-up decision — PR-F5: non-negative typed API body budgets (2026-08-25)

**Decision: extend `ApiRequestBody` with one opaque `ApiRequestBodyByteLimit`, checked from a
`Natural` against the private WAI reader's `Int` range.** Buffered, URL-encoded, and streaming
declarations previously accepted raw signed `Int` budgets, leaving negative declarations to be
interpreted incidentally and differently by readers. The endpoint declaration already owns body
consumer selection and resource bounds, so a separate runtime validation layer would duplicate that
ownership. The smart constructor rejects values beyond `maxBound :: Int`; only the three private
reader adapters convert a valid value to `Int`. Thus zero and ordinary values remain valid, oversized
bodies retain their existing typed 413 behavior, and neither negative nor overflowed public budgets
can be authored.

### Follow-up decision — PR-S1: durable security time is Unix epoch time, not process uptime (2026-08-23)

**Decision: extend the existing `AccountWorkflow` clock seam with
`HarchWeb.Time.UnixTimeNanoseconds`, and derive RFC TOTP Unix seconds from
that one operation instant; do not introduce a second ambient clock or persist
a monotonic reading.** The existing workflow clock already owns the values
that registration, verification, login throttling, ordinary sessions, MFA
enrollment sessions, and profile authorization share. Its old
`getMonotonicTimeNSec :: IO Word64` implementation was appropriate only for
elapsed request/operation duration, but its values were stored in PostgreSQL
and compared after a restart or on another host, where their boot-relative
origin has no common meaning. Replacing the clock's value type—not merely its
runtime implementation—makes passing a monotonic `Word64` to durable state a
type error. `UnixTimeSeconds` similarly makes the RFC 6238 boundary explicit;
the workflow derives it purely from the same `UnixTimeNanoseconds` read, so an
independent read cannot cross a time-step boundary between persistence and
TOTP validation.

The type is deliberately not used for server/request telemetry: those remain
monotonic durations, where wall-clock adjustment would be wrong. This extends
an existing ownership boundary rather than adding a general time service or a
parallel account-workflow abstraction. The new versioned
`epoch-security-time-v1` migration cannot translate prior stored values—old
numbers have no recoverable epoch—so it deletes verification tokens,
login-attempt history, and both bearer-session tables, including sessions a
client might replay after its browser's `Max-Age` has elapsed. It assigns the
migration transaction's Unix epoch only to non-authorizing historical markers
whose state must be retained (created/verified, TOTP-confirmed, and
recovery-code-used). Focused coverage proves both a raw copied cookie is still
rejected after a simulated clock-origin reset and the RFC 6238 SHA-1 vectors at
known Unix instants.

### Follow-up decision — PR-S5: bounded login-attempt reservations (2026-08-24)

**Decision: extend the existing PostgreSQL login-attempt reservation lifecycle,
not a parallel cleanup worker or a second aggregate table.** PR-S4 already made
the reservation function the one atomic admission boundary.  Its next
reservation now takes a short-lived global capacity lock, deletes rows outside
the application-selected retention window, applies the global row ceiling, and
only then inserts a provisional failure.  A successful settlement or an
explicit cancellation deletes that provisional row; a failed or process-abandoned
attempt stays only until a later admission performs the same in-transaction
prune.  This gives stale-reservation cleanup one owner and means concurrent
keys cannot race past the capacity budget.

The `web-api` application owns the policy (100,000 rows and the normal
15-minute login window); the PostgreSQL adapter's default builder selects it,
and an explicit policy builder plus smart constructor support alternative
deployments/tests.  The adapter rejects keys over 260 characters before issuing
SQL, while the general framework-owned `EmailAddress` parser now enforces the RFC 5321 254-ASCII-octet
mailbox limit.  That small universal validation primitive is the missing
framework capability rather than an application-only workaround: every email
transport and account workflow benefits from the same valid representation.
The table also has a database check as defense in depth.  Capacity exhaustion
is an ordinary typed store-unavailable outcome and fails authentication closed;
it does not silently skip the throttle or discard a live reservation.

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

### Decision record — EJ: shared Argon2 work admission (2026-08-24)

**Decision: add the small general `HarchWeb.Password` primitive
`PasswordWorkBudget`/`PasswordWorkGate`, and retain one 512-MiB gate in the reference
application, rather than extending the WAI-wide request-concurrency limiter or adding an
application-local queue.** The existing inbound-resource controls own bytes and broad in-flight
request counts, not the validated cost of a particular native password operation; changing their
meaning would couple unrelated routes to login load. Argon2 cost parsing already belongs to the
password boundary, so a KiB-weighted gate is both a narrowly reusable framework capability and the
only boundary that can reserve the actual validated stored-hash cost. It also caps each gate at
eight concurrent operations, so low-memory valid hashes with a higher iteration count cannot evade
the handler-side CPU bound.

Admission is non-queueing: when remaining capacity cannot cover a hash, the handler receives a
typed, opaque temporary-unavailable outcome immediately. This prevents attacker-held queued work
from becoming unbounded latency; a generic elapsed-time timeout would only report after capacity
was already committed and cannot reliably interrupt native work. Reservations cover registration
hashing, known and unknown password verification, and each recovery-code verification, and release
through `finally` on ordinary or asynchronous exit. The reference application's process-wide gate
preserves its pure runtime-builder API while making independently constructed runtime workflows
share the same cap. This closes EJ's handler-side bounded-admission scope; the separate **P-S6**
follow-up continues to own registration retry and cleanup lifecycle work.

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

### Decision record — PR-S6: bounded, retryable pending registration delivery (2026-08-24)

**Decision: extend `WebApi.Account`'s existing pending-account and verification-store boundary
with an atomically staged delivery claim, rather than add an application-local mail queue or a
second account lifecycle.** Registration now reserves EJ's existing shared `PasswordWorkGate`
before it runs Argon2id, then submits the hashed candidate to one PostgreSQL staging function. That
function serializes capacity, email, and username decisions with transaction advisory locks; it
removes expired unverified accounts, enforces the application-owned 100,000 pending-account cap,
and either creates a delivery claim, claims a retryable pending account, or returns the established
opaque already-registered outcome. A successful SMTP send settles the matching digest claim;
an exception or ten-second transport deadline releases it, and an uncompleted claim becomes
reclaimable after five minutes. This is a staged retry lifecycle whose persistent data remains the
already-owned `accounts` plus `email_verifications` records, not a parallel outbox with competing
cleanup rules. The client receives exactly the same status, headers, and encoded action body for a
new, retryable, or already-registered address; only private low-cardinality `created`, `retried`,
and `already-registered` lifecycle diagnostics differ. SMTP timeout is likewise a distinct private
`account.registration.delivery-timeout` failure code (rather than a string matched out of a generic
transport exception), so it is alertable without retaining recipient or token data.

**Password-work inventory:** the only unauthenticated attacker-reachable native password work is
registration hashing, known/unknown password verification, and recovery-code verification. All three
now reserve the same process-wide gate. Recovery-code *hashing* occurs only after a verified account
has reached its authenticated MFA-enrollment workflow, and hashes server-generated recovery values;
it is not an unauthenticated caller-controlled KDF surface. No other production caller reaches
`hashPassword`, `verifyPassword`, `hashRecoveryCode`, or `verifyRecoveryCode` outside those paths.

### Decision record — DM: opportunistic password-hash migration (2026-08-25)

### Decision record — AHI-2: durable staged verification resend (2026-09-01)

**Decision: extend `AccountStore` with an account-verification-specific
reserve/complete/release claim lifecycle, instead of a generic keyed budget or
a separate mail queue.** The account store already owns pending state and the
active verification token. A PostgreSQL reservation writes a candidate into
`verification_resend_claims` while deliberately preserving the currently
delivered `email_verifications` row; completion atomically promotes the
candidate and appends one `verification_resend_deliveries` record. Release,
lease expiry, and the rolling-window prune remove only the candidate/history
they own. This is consequently safe when SMTP fails or async cancellation
arrives at either handoff: the old token remains valid, and an abandoned claim
is reclaimable after five minutes.

The policy is application-owned and bounded (three deliveries per account per
hour, one live claim, and 100,000 combined claim/history rows in the reference
application). PostgreSQL transaction advisory locks serialize the global
capacity/prune decision and the account lifecycle decision. The public action
maps delivered, throttled, and no-longer-pending results to one generic 202
body; the Haskell ADTs retain only closed lifecycle classifications, never an
email, token, row id, or provider exception. This narrow extension remains
separate from AHI-3: login attempts have a different principal, retention
policy, and settlement meaning.

### Decision record — AHI-3: keyed authentication reservation groups (2026-09-01)

**Decision: extend `HarchWeb.Security`'s existing trusted-forwarding resolver
with an opaque `ClientAddress`, then extend the existing `LoginAttemptStore`
reservation into one typed, non-empty group of principal and peer scopes.**
The framework already decides whether a socket peer may supply forwarded
context; a second web-api parser would risk accepting a spoofed header or
giving observability and authentication different meanings. `ClientAddress`
therefore uses that same resolver, normalizes a bounded address-shaped trusted
token, and falls back to the accepted socket peer on absent or malformed input.
It enters `AppRequestContext` through the existing context-enrichment path;
application workflows never read forwarding headers directly.

At the application boundary, known aliases use an account-and-stage principal,
unknown identifiers use their validated canonical identity, and every attempt
also has a stage-independent trusted-peer scope. `LoginAttemptBudgets` orders
and deduplicates this closed set before it reaches PostgreSQL. One opaque group
reservation owns all child scopes for admission, failure settlement, and
cancellation; this preserves the existing masked ownership handoffs without a
custom monad or compensating multi-store calls. PostgreSQL locks the group
capacity then scopes in canonical order and counts logical groups for its
100,000-row limit, avoiding a silent capacity-halving when normal attempts
gain two scope rows. Raw values stay database-only and are never public,
metric, or log labels.

**Decision: extend the existing account-credential store with an atomic
compare-and-replace operation, rather than add a migration table, a background
worker, or a second login path.** After a bounded, valid legacy Argon2id hash
verifies, the password-login workflow may spend one separately admitted current-policy
hash and asks the store to replace only the account's same old hash. A concurrent
successful login or password change therefore produces a harmless no-op instead of
overwriting newer credentials; only one request upgrades a particular old hash.
The rehash happens before the existing email/MFA classification, but failures to
admit the optional extra work, native-hash failures, and store failures do not alter
the authentication result: accepted legacy hashes stay valid, and a later successful
login can retry. A stored policy is eligible only when every Argon2 cost is no greater
than the target and at least one is lower; a mixed stronger/weaker policy is retained
so migration never lowers an existing cost. This is a narrow application credential
lifecycle extension, not a new framework credential protocol.

### Decision record — DM: ACME temporary-state ownership and runtime challenge authority (2026-08-25)

**Decision: keep both protections at the existing ACME runtime/challenge boundaries, rather than
adding a diagnostic retention path or a fallback challenge matcher.** Certbot's temporary state
directory is created by `prepareCertbotManualTlsBindPlanWithLogger`, which is therefore the only
layer that can guarantee deletion if directory setup, certificate-name validation, process launch,
certbot, certificate validation, or certificate publication throws. The successful return transfers
that directory's cleanup ownership to `RunningAcmeRuntimeServer`; every earlier exception removes it
immediately. Failure diagnostics retain the directly relevant executable/exit/stdout/stderr result,
but do not keep a directory containing the ACME account key or copy its log content into an exception.

Runtime HTTP-01 challenges now require the configured domain to equal an actually supplied `Host`
value (after the existing port stripping). A hostless request cannot establish entitlement to a
domain-specific challenge, so it is an ordinary non-match. This is a narrow correction to the one
existing challenge matcher; no new routing policy or hostname inference is introduced.

### Decision record — DQ: peer-address attribution before a rejected TLS handshake (2026-08-28)

**Decision: extend the existing `HarchWeb.Server.Transport` boundary with Warp's public
`setAccept`/`setFork` lifecycle hooks.** The old FIFO bridge from `onOpen` to `setFork` was
demonstrably incorrect because a rejected TLS connection never reaches `onOpen` and could inherit a
prior peer. `setAccept` instead records the accepted kernel `SockAddr` before WarpTLS creates its
connection maker; `setFork` claims that one-place handoff and registers the worker's address before
TLS setup. The transfer uses non-blocking operations: a full/empty slot is an explicit, safe
lifecycle-contract failure, never guessed observability. Only the accept-loop thread clears an
unclaimed peer after its own setup failure, so a worker exception cannot erase a newer peer. This
extends the existing public transport API rather than adding a server implementation, vendoring
Warp, pinning an unreleased revision, or changing TLS/HTTP2 ownership. Real distinct-source,
sequential, concurrent, plaintext-on-TLS, premature-close, and asynchronous-worker regressions
prove the peer attributes remain tied to their accepted TCP connection.

### Decision record — PR-SEC1: cancellation-safe multipart ownership handoffs (2026-08-28)

**Decision: extend `HarchWeb.Api.Multipart`'s existing scoped-upload lifecycle with masked
handoffs and continuation-based promotion; do not add another storage adapter or expose a raw
completed value.** A staged upload already has one explicit framework-owned cleanup reference, and
an unpromoted completed upload already has one scoped cleanup list. The defect was the transfer
between those two existing owners: cancellation could occur after completion had cleared the staged
reference but before the scoped list owned the value. Completion now registers the opaque upload in
the scoped list before clearing staged ownership, under one masked handoff. The completion action
remains interruptible where its storage adapter performs interruptible IO; before that handoff the
staged reference remains the cleanup owner.

The old `IO (Maybe stored)` promotion result could be claimed and then interrupted before a caller
adopted it. `withPromotedMultipartUpload` instead runs an adoption continuation under a masked
claim: normal return transfers ownership to the application, while an exception or cancellation in
the continuation discards the completed upload exactly once. The continuation must not publish a
second concurrent owner before it returns normally. This preserves application-selected storage,
the single multipart parser/dispatcher, and the existing explicit promote-or-discard lifecycle
while making both transfer boundaries cancellation-safe. Focused asynchronous regressions prove
completed-unadopted and claimed-but-cancelled uploads each invoke their adapter discard exactly
once.

### Decision record — PR-SEC2: principal-wide password-attempt throttling (2026-08-28)

**Decision: extend the existing password admission path with an opaque resolved-account key; do
not add an alias map or a parallel throttle.** The single credential lookup already returns the
stable `AccountId` required to make email and username spellings share one durable failure budget.
Known credentials therefore reserve `account:<id>` before their password verification, while an
unknown identifier reserves its canonical submitted-identifier key and verifies the same fixed
dummy hash. This preserves bounded unknown-identifier admission and rejection timing without
turning the attempt store into credential storage or adding an inconsistent second lifecycle.

Credential lookup must precede admission because there is no principal key until it completes. A
lookup failure consequently returns the existing typed credential-store failure without creating a
reservation; after resolution, the unchanged admission/settlement lifecycle owns all password work.
Concurrent email/username regressions prove the configured account-wide maximum is shared and the
next alias attempt is throttled. PR-SEC3 remains responsible for making the post-reservation
cancellation handoffs themselves safe.

### Decision record — PR-SEC3: cancellation-safe login-attempt ownership handoffs (2026-08-28)

**Decision: extend `WebApi.Login`'s existing `LoginAttemptStore` lifecycle with a masked handoff;
do not create a second reservation type, wrapper, or uninterruptible database layer.** The existing
store-issued reservation is already the sole owner of the provisional durable failure. The fault was
between its operations: an asynchronous exception could arrive after admission returned but before
the work cleanup handler existed, or after work returned but before settlement began. The lifecycle
now masks those short ownership transfers, then restores asynchronous exceptions for admission,
password/MFA work, and settlement. Cancellation during the restored work or settlement invokes the
same store cancellation operation; a normal no-settlement outcome still reports a typed cancellation
failure through the existing result path.

This deliberately does not make a blocked database operation uninterruptible and does not claim a
process crash or cancellation while compensating cancellation itself is blocked can be recovered in
process. Those cases retain an unsettled row for the existing retention cleanup (PR-S5). Deterministic
regressions cover cancellation directly after an admitted reservation is observed and during the
post-work settlement entry, ensuring neither normal in-process boundary can leave a stale reservation.

### Decision record — PR-SEC4: redact provider and persistence payload diagnostics (2026-08-28)

**Decision: redact at the existing typed adapter error boundaries; do not introduce a second logging
abstraction or let action handlers attempt to scrub arbitrary text.** PostgreSQL decoders retain a
stable row count and per-row column counts when their result shape is invalid, but never preserve a
returned value in the error text. `PostgresRunnerError` uses the same representation when it is
rendered through an ordinary `Show` path. SMTP and Gmail delivery failures retain only the protocol
status/category, never a server response line or provider response body. Generic application-selected
email-adapter exceptions become one closed transport-failure category. OTLP sends use a no-body HTTP
response path and retain only a rejecting collector status, never its body. This keeps the useful
low-cardinality diagnosis at the point that knows which input is untrusted, before existing account
action diagnostics and application log reporters can carry it.

The alternative of redacting in every caller would be incomplete by construction: typed failures
already flow through registration, login, MFA, profile, setup, and observability paths. Keeping the
redaction beside each adapter also leaves normal client-facing error handling and stable failure codes
unchanged. Sentinel regressions exercise a password hash, encrypted MFA/recovery data, account
email/profile data, SMTP response lines, and a Gmail provider body through their error/diagnostic
boundaries, proving neither a public response nor its attached private log entry retains the value.

### Decision record — PR-SEC8: typed OTLP transport diagnostics (2026-08-30)

**Decision: extend `HarchWeb.Observability.Otlp` with a closed export-failure type at its existing
HTTP adapter boundary; do not redact `http-client` exception text in the application worker.** An
OTLP exporter intentionally accepts endpoint queries and arbitrary configured headers, either of
which can be credentials. `http-client`'s rendered request exception does not promise to redact
those values. The adapter consequently maps invalid endpoints, transport exceptions, and rejecting
collector status to payload-free ordinary results before its caller can observe a rendered
exception. The application worker recognizes that closed type and logs its stable category; any
unexpected exporter exception receives one generic fallback category rather than being rendered.

This extends the one existing OTLP transport boundary and leaves the bounded queue, manager
ownership, exporter configuration, and normal request path unchanged. A second logging-redaction
layer would be incomplete because every future OTLP caller could bypass it. Sentinel coverage drives
a failed request with an `X-API-Key` value and endpoint query credential through the adapter and the
worker's final log-message formatter, proving neither secret appears; the existing status/body test
continues to prove collector response bodies cannot enter diagnostics.

### Decision record — DT: configurable modern TLS server policy (2026-08-26)

**Decision: extend the existing listener `TlsConfig` and its manual/ACME bind plans with one closed
protocol-and-cipher policy, rather than rely on package defaults or add a separate runtime override.**
The default permits only TLS 1.2/1.3 and a browser-oriented AEAD/PFS set. Environment configuration
may explicitly select an older protocol and one of the installed library's closed cipher identifiers,
but parsing rejects empty, duplicate, unknown, and version-incompatible selections before a socket is
opened. This keeps untrusted deployment text at the existing configuration boundary and makes manual,
shared-certificate, and ACME listeners obey exactly the same policy. The explicit legacy escape hatch
is deliberate compatibility authority, not a silent fallback to `tls` defaults; future package upgrades
remain HSEC-driven work instead of a blocked dependency-pin task.

### Decision record — AW: authenticated SMTP uses fresh TLS validation (2026-08-26)

**Decision: keep certificate-chain and hostname validation fresh for every SMTP connection, rather
than add a validation-result cache or weaken the exact coverage gate.** The expected SMTP connection
rate does not justify a cache's trust-store, revocation, certificate-rotation, ownership, expiry, and
invalidation semantics. The TLS setup must say beside the client construction that it deliberately is
not optimized for validation caching or rapid reuse, and a future cache requires a new ADR. A local
SMTP/TLS listener belongs in the verification path for actual STARTTLS/implicit-TLS, multiline, and
capability behavior, never as a fabricated coverage mechanism. If fresh validation cannot be
represented in a coverage-safe supported API shape, record the missing capability or seek it
upstream; do not introduce a coverage exception or semantically empty cache. The one explicitly
approved exception is the documented @ByteString.empty@ strictness at the TLS library boundary:
it evaluates the library-required service-identity argument without creating a cache or changing
certificate validation, after the supported construction and real-listener tests had been tried.

### Decision record — DS: static representation metadata and file-backed delivery (2026-08-25)

**Decision: keep static-file ownership at the configured root boundary, and extend that one
dispatcher with standard representation semantics rather than introduce an asset-serving runtime or
an application-visible file abstraction.** A request may select only a safe relative path; the
dispatcher still canonicalizes the candidate against the configured root before obtaining metadata
or handing its canonical path to Warp. Successful `GET` responses are `responseFile` values, so
the server can use its file/sendfile path instead of allocating one in-memory `ByteString` per
request. The response derives a weak ETag from the file size plus high-resolution modification time,
emits `Last-Modified` and `Accept-Ranges: bytes`, accepts `If-None-Match` (including weak matches)
before `If-Modified-Since`, and supports one satisfiable byte range through `FilePart`; unsatisfied
or malformed ranges receive 416. `HEAD` preserves the selected representation's status and headers
without sending a body. These are transport representations of application-owned files, not a new
untrusted-input storage lifecycle, so the multipart storage-adapter rule does not apply.

**Root-prefix policy:** a `staticUrlPrefix = "/"` root owns only paths with an explicitly configured
content type. Unmapped paths fall through to the route table instead of producing a static 404 before
routing; deliberately allowlisting the empty extension remains an explicit choice to make
extensionless paths static. Static misses retain their plain 404 but deliberately omit
`Cache-Control`, preventing a shared cache from pinning a missing deployment artifact for the
successful asset TTL.

### Follow-up decision — CO: no global 108-column source limit (2026-08-25)

**Decision: remove the unenforced 108-column target rather than turn it into a formatting gate.**
The target had no project policy or tooling behind it, while 3,298 existing source lines exceeded
it. A global width check would force broad, mechanically motivated rewrites and would still be a
poor proxy for readability in declarative markup, records, type signatures, and tests. Pinned
Ormolu remains the formatting authority. Reviewers should instead use the existing compositional
and module-health rules: extract a cohesive responsibility when ownership is obscured, preserve
direct total ADT folds, and do not split declarative tests merely to satisfy a metric. A future
line-length policy needs a concrete readability failure and a formatter-compatible enforcement
plan; it must not be reintroduced solely as a count-based gate.

### Follow-up decision — EI: additive ICU-backed localization, not locale routing (2026-08-25)

**Decision: add `HarchWeb.Localization` as a separate framework primitive with an
application-supplied message-key lookup, backed by ICU4C `MessageFormat`.** Existing routing owns
only locale selection; changing it to own message catalogs would conflate a request-address concern
with an application's copy and translation ownership. The new `Localizer` is therefore an additive
layer: an application maps its closed (or extended) key type plus locale to an optional template,
then the framework renders named text and number arguments through ICU's CLDR rules. This is option
1 of the missing-framework-capability protocol: internationalized message formatting is a general
framework concern, while the catalog remains application-owned.

`HarchWeb.Localization.Quasi.message` rejects structurally invalid ICU argument braces during
compilation and is implemented through the established Template Haskell execution boundary. The
reference application's `AppMessage` enum and catalog replace the positional two-language action
and SSR-page helpers; the independent `examples/localization` package proves Icelandic plural HTML
and a localized in-memory-adapter API failure. ICU remains the runtime authority for complete
MessageFormat semantics, while the quasiquoter deliberately catches structural authoring errors
early rather than claiming type-level validation of every application-specific interpolation record.

### Follow-up decision — AHI-7: structural accessible fields and typed focus (2026-08-31)

**Decision: extend the existing markup, control, and client-action response boundaries; do not add a
form-builder DSL.** The registration experiment and the separate login form reproduced the same
inseparable relationship: a visible label targets one native control, while optional hint and error
nodes contribute stable IDs that the control must reference in order. `HarchWeb.Controls` therefore
owns a higher-order `accessibleField` renderer and non-empty linked error summaries, while each
application continues to own its validation ADTs, localization, control choice, and layout. The
closed markup vocabulary adds typed ID-reference, invalidity, fragment-link, and focus attributes
instead of exposing a generic unchecked attribute constructor.

Client-action focus now carries `ElementId` through the existing response record and converts to
text only in the JSON encoder. Reference-app account field IDs are shared typed constants between
rendering and workflows, so a patch producer cannot silently drift to an arbitrary string. Pure,
independent registration checks use a small applicative accumulation type in declaration order;
the existing effectful account workflow remains fail-fast. One invalid field focuses that control,
multiple invalid fields focus the ordinary labelled summary, and the summary deliberately has no
`role=alert`: focus supplies the announcement without a duplicate live-region event. Complete form
regions are not live regions; success uses a polite status, failures use an alert, and the existing
control-local action lifecycle remains responsible for pending and delayed progress.

The linked summary also exposed a navigation-boundary rule: same-document fragment history remains
native browser behavior. The progressive-navigation runtime records the document URL represented by
the current shell and ignores a `popstate` whose origin, path, and query still identify that document.
Otherwise activating a plain `#field-id` link can fetch and replace the page, discard the patched
invalid values, and defeat the focus target the accessible control deliberately supplied.

### Follow-up decision — AHI-9: application-owned authentication semantics (2026-08-31)

**Decision: keep autocomplete vocabulary and authentication form models in the
application, while adding only the missing general native `selected` attribute
and real-browser test operations.** The existing open `autocomplete :: Text`
boundary already represents the evolving HTML token grammar. A framework enum
covering only account fields would create false completeness, so `web-api`
owns named token constants and an audited control table instead.

Login now has a closed `LoginProofChoice` and separate TOTP and recovery-code
fields. Independent identifier, password, and selected-proof syntax uses a
shared application-local applicative `Validation`; this is intentionally not
a monad because fail-fast bind cannot accumulate independent errors. A valid
request then enters the existing `AppM` effect rail once. Identifiers and the
non-secret choice survive a patch, while password, TOTP, recovery, verification,
enrollment-secret, and generated-code values are excluded from response models
and diagnostic `Show` output.

Preserving a native select choice across SSR patches demonstrated one small
closed-markup gap, so `HarchWeb.Markup.selected` extends the existing boolean
attribute vocabulary and the quasiquoter lowers it directly. Paste and narrow
viewport operations extend only the test adapter: application scenarios still
own assertions and control flow. Real-browser proof covers clipboard input,
autofill-compatible replacement events, delayed runtime capture, keyboard
order, focus at narrow/zoomed layout, secret clearing, and the explicit
scripts-disabled `method=dialog` policy. The complete inventory and limits of
that proof are recorded in `docs/accessibility.md`.

### Follow-up decision — AHI-8: pluggable accessible navigation lifecycle (2026-08-31)

**Decision: extend `PageShell` and the existing replaceable
`NavigationRuntime` with a declarative lifecycle adapter; do not add a second
navigation dispatcher or accept application JavaScript callbacks.** A shell
may declare a typed focus target, a document-title or element-text
announcement source, a localized native skip link, and application-owned CSS
classes. `mainNavigationLifecycle` supplies the reference default: the stable
typed main ID is programmatically focusable, the skip link is the first body
control, and one fixed polite/atomic status receives the destination document
title. Harch fixes the live-region semantics and runtime ordering so an
adapter cannot accidentally contradict the accessibility contract.

This split is deliberately pluggable at both established boundaries. An
application can use `FocusElement` and `AnnounceElementText` when its replaced
main or navigation region owns a stable alternative; a missing or
out-of-region target makes the fetched document incompatible and uses native
navigation. Applications needing a different completion algorithm can already
replace `NavigationRuntime` as a whole. Selector strings, arbitrary status
attributes, and script callbacks are therefore unnecessary new surfaces.
`PageShell` and `Document` now carry the existing opaque `ElementId` for their
main IDs instead of raw text.

The one runtime validates the final same-origin `Response.url`, document
regions, and lifecycle bindings before replacement; commits that final URL to
history; then focuses with `preventScroll`, scrolls deliberately, and performs
one status-node mutation. Back/Forward uses the same function. A new
navigation aborts and supersedes the older fetch, so the older response cannot
move focus, add history, or announce. Failed, malformed, cross-origin, and
incompatible responses retain native hard-navigation fallback and do not
announce success. The `web-api` shell supplies English/Spanish copy and
app-owned focus/visually-hidden CSS; real-browser tests cover redirect,
history, overlap, narrow 200% zoom, delayed runtime, scripts-disabled skip
navigation, and every fallback class.

### Follow-up decision — AHI-6: replaceable dialog runtime; app-owned floating Help link (2026-08-31)

**Decision: extend the existing shell/early-response seam with ordered generic
`RuntimeAsset` values and add only a typed native-dialog control; do not add a
dialog-specific server configuration or a general floating-action API.** The
language experiment exposed a framework capability gap: `PageShell` could
declare deferred modules, but request execution could serve only the
navigation runtime. `Site.siteRuntimeAssets` and
`Application.applicationRuntimeAssets` now carry the same application-selected
module values used to build shell descriptors. The existing early response
boundary serves the first declared asset that owns a path. An application can
choose `defaultDialogRuntime`, substitute another module with the same capture
contract, or omit dialog enhancement while retaining the complete SSR
language route.

The framework surface is semantic rather than product-specific.
`DialogControlProps route` renders a named, always-dismissible native
`<dialog>` and retains the trigger's typed route until
`dialogControl :: (route -> SafeUrl) -> ...` receives the application's route
renderer. This matches `pageLink`: Harch owns the route-aware control contract
without selecting an application's route algebra, URL policy, locale context,
or path syntax. The resulting native-link fallback is available before or
without the adapter. The capture kernel owns a bounded early-activation
recovery; the default adapter owns `showModal`, initial and contained focus,
Escape/visible close, connected-invoker restoration, and navigation cleanup.
It does not own the application's languages, placement, or visual design. A
speculative button trigger and required-decision dialog were omitted because
this reference case could not prove their fallback and dismissal invariants.

The Help reference case found no corresponding framework gap. Existing typed
native anchor composition plus the new non-empty `AccessibleName` used by the
dialog expresses its destination, name, decorative icon, and localized visible
label without an additional wrapper. `web-api` therefore owns the floating
placement, safe-area spacing, target size, responsive label, and destination
suppression. This completes the approved link use case only. A future
command-style FAB must be a native button attached to a demonstrated client
action whose immediate capture path already exists; it is not a hidden variant
of this navigation component.

Every row's `State` follows the "Naming a partial slice" convention above: `Implemented` means
the full designed scope shipped; a partial slice must say so and name its follow-up.

| Area | State | Guidance |
| --- | --- | --- |
| Complete SSR and enhanced navigation | Implemented | `NavigationLifecycle` is the optional declarative accessibility adapter interpreted by the existing replaceable runtime. The reference adapter focuses the stable main, commits final same-origin redirect URLs, and announces the destination title once; direct loads, incompatible responses, delayed modules, and scripts-disabled links retain native behavior. |
| Immediate modeled-form capture | Implemented | Extend the kernel contract before adding another enabled framework event type. |
| Generated static page algebra/dispatch | Implemented | Export `pageDefinition`; keep API and dynamic routes explicit. |
| Typed markup and component calls | Implemented | Prefer named record fields; reserve positional `props` for distinct typed values. |
| Scoped CSS names | Implemented | Use `cssScope`; typed CSS authoring remains future work. |
| Configured static assets | Implemented | Successful assets are canonical-root-checked file responses with weak ETags, `Last-Modified`, conditional 304s, single-range 206/416 semantics, and `HEAD` metadata; static 404s are never cacheable. |
| Declarative client actions and region patches | Implemented | Declare `ActionCodec` endpoints once; render forms and dispatch from it, then mutate with typed action responses and `RegionPatch`, not page POST/reload workflows. |
| SSE live updates | Implemented | Start from meaningful SSR content; treat streaming as an enhancement. |
| PostgreSQL and custom adapters | Implemented | Keep operations typed and interpreters app-selectable. Runtime queries share a bounded `WebApi.Postgres.Pool` instead of one connection per query. `DatabaseTransportSecurity` exposes the closed libpq TLS modes; an omitted setting deliberately preserves libpq's own resolution/default (currently `prefer`), while an explicit `verify-full`/CA policy flows to runtime, migration, and `psql`. A real PostgreSQL 17 fixture proves verified success and the untrusted-CA, hostname-mismatch, and TLS-disabled failures. Migrations no longer run per-statement `psql` subprocesses (AX). |
| Auth, sessions, MFA, localization, telemetry, TLS, and proxy support | Implemented | Auth, sessions, MFA, telemetry, TLS, and proxy support are complete. `HarchWeb.Localization` provides ICU-backed application lookup, CLDR rendering, a structural compile-time template quasiquoter, and an extendable empty framework-default layer; `web-api` uses its closed catalog and `examples/localization` proves Icelandic SSR pluralization plus a localized in-memory-adapter API error (EI, 2026-08-26). Login-attempt reservations retain only the application-owned 15-minute window, delete successful/cancelled rows, cap storage at 100,000 rows, and reject oversized keys before persistence (PR-S5, 2026-08-24). Argon2 admission is a shared, non-queueing 512-MiB KiB-weighted gate with an eight-operation CPU-concurrency ceiling across registration, password login, and recovery-code verification (EJ, 2026-08-24). |
| `HarchWeb.Api`/`HarchWeb.Api.Endpoint` typed endpoints (buffered, URL-encoded form, multipart, and streaming request bodies) and closed route-family registry (`RouteFamily`/`combineRouteCodecs`/`apiRouteEndpointFamilyCodec`/`apiRouteEndpointFamilyDefinition`) | Implemented | `examples/custom-api` and `examples/multipart-upload` use the route-family registry; `web-api` uses its existing single dispatcher with `apiRouteDefinitionWithContext` for `/api/status` and `/api/second`. `ApiEndpointContract` groups method, fields/body, representations, and field-failure policy; a path-owning `ApiRouteEndpointDeclaration` is used only for context-free routes, while context-aware definitions reuse the contract (PR-F6, 2026-08-25). `ApiResponse` carries observability attributes/log entries. The unused compatibility middleware/table was deleted, and `HarchWeb.Api.Endpoint` is now a public facade over private declaration, family, and runtime modules. |
| `HarchWeb.Api.Multipart` bounded streaming consumer, in-memory default, and native upload form | Implemented | Storage ownership/cleanup, bounded in-memory default, media-type/boundary validation, preamble/header/body/declared-length bounds, bounded scanner state, untrusted filenames, and scripts-enabled/disabled native-upload cleanup are implemented. The consumer deliberately stops reading after cleanup rather than draining; that is the documented WAI transport policy, to be revisited only if a concrete backpressure problem is observed—not an unowned partial implementation. |
| Declarative dynamic path/query templates | Design direction | Use explicit typed codecs until the route-template DSL is executable. |
| Typed page-local CSS/JavaScript EDSLs | Design direction | Keep current assets narrow, deferred, and route-aware by convention. |
| Automatic database-to-live-view subscriptions | Design direction | Use explicit SSE today; do not imply automatic subscriptions exist. |

The design-only route syntax is isolated in the
[route-templates guide](../examples/route-templates/README.md). It must remain labeled as design
direction until its path/query DSL is executable and tested.

### Decision record — AHI-4A: typed endpoint admission rail (2026-09-01)

**Decision: extend the existing post-match route dispatcher with one typed
endpoint-admission rail; keep request middleware, route concurrency, action
body decoding, and handler invocation in their established owners.**

Endpoint metadata is a pure lower-level declaration shared by `RouteDefinition`
and `ActionCodec`: a validated endpoint name, route template, protocol, and
explicit access requirement. Separating that declaration module from the
response-bearing guard result is deliberate: `ActionCodec` is below
`Response`, and importing the full guard API there would form an import cycle.
The pure split preserves typed metadata instead of replacing it with raw action
paths or a string property bag.

The one request executor first resolves a route/method (or a declared action
method/path), then runs guards in declaration order, and only then performs
route concurrency admission, action body intake/decoding, or a handler. A
guard may enrich the typed context or halt with the existing `Response`; it is
never given a handler continuation. `RouteNotFound` remains outside this rail,
while HEAD, OPTIONS, and 405 retain the selected declaration. An explicitly
public root fails closed when presented with a protected declaration, rather
than treating an empty guard list as an authentication policy.

`AuthenticationPipeline` keeps proof extraction, verification, current
principal establishment, authorization, attachment, and challenge rendering
as replaceable typed adapters. Expected outcomes run on `ExceptT
AuthenticationFailure IO` and are interpreted once at the endpoint boundary.
In particular, signature validity is not principal validity: a durable session
or account resolver must still reject revoked/disabled principals. The JWT
adapter uses `jose-0.12`, selected after the compatibility experiment because
it supports the repository's current `crypton ^>=1.0.6` and GHC 9.14; it
requires an explicit subset of HS256, HS512, RS256, and RS512 rather than the
library default, and excludes `None`. This is the framework capability
implemented by AHI-4A; the reference application's durable account/JWT
configuration and typed login return navigation remain AHI-4C work.

`HarchWeb.SecurityEvent` now owns validated root route observations, closed
authentication/authorization/session event bodies, delivery requirements, and
a deliberately narrow telemetry projection. It cannot carry a raw path,
query, proof, JWT, claim, header, cookie, account/session ID, locale, or
application audit payload into telemetry. Applications retain the only
durable-audit conversion and transaction policy, so an undelivered
`AuditRequired` event is explicit rather than reported as successful. AHI-4B
extends the already-shipped root observation to a trusted module mount chain;
it does not create a parallel event vocabulary or let a child forge route
attribution.

The root configures `SecurityEventRoot` once: its validated module name,
context-to-locale function, application delivery adapter, and bounded
undelivered-event health hook. After route/method selection, the shared
matcher attaches the derived sink to `EndpointRequest` before any guard runs.
The sink accepts only an `EventDeliveryRequirement` and closed `SecurityEvent`
body, so neither an authentication adapter nor an application pre/post guard
can replace the matched endpoint, declared template, root module, or locale
with request text. Best-effort authentication and authorization facts still
receive truthful delivery results and report failed delivery through that hook;
an application choosing an audit-required operation owns its transactional
interpretation. This keeps the AHI-4A framework boundary pluggable without
pre-choosing the AHI-5 PostgreSQL audit schema or schedule.

### Decision record — AHI-4B: structured route locations and modules (2026-09-02)

**Decision: extend the existing `RouteCodec` and its one request executor with
a structured `RouteLocation`; do not add a raw-text compatibility codec, a
mounted WAI application, or another action dispatcher.** The audit confirmed
that the current raw `Text` target is split and decoded again by application
code (`WebApi.Route`) after the server has already selected its route boundary.
That representation cannot truthfully distinguish an unmatched path from a
malformed percent-encoded path, and makes a typed child mount impossible
without each child re-parsing a prefix. `RouteCodec` already owns rendering,
path recognition, method selection, and family precedence, so it is the
correct sole owner of decoded path segments and query fields.

The migration therefore makes the HTTP adapter decode one bounded request
target into `RouteLocation`, then passes only that structured value to codecs.
`RouteParseResult` preserves `RouteNotMatched` separately from a typed decode
failure, so a malformed location cannot fall through to a later mount. Rendering
encodes the same components once into `SafeUrl`. Route/action/form ownership
stays disjoint: route locations own path/query values; `ActionCodec` continues
to own action fields and bodies. The temporary source migration may adapt each
existing route table while the worktree is incomplete, but no raw-text codec
surface ships or is documented as a second supported API.

Adapters use the shared `mapRouteParseResult` to map only a parsed route while
leaving both an ordinary ownership miss and malformed location intact. This is
the small extension of the existing route boundary that prevents each mount or
static adapter from reimplementing a subtly partial-looking result mapper.

`ApplicationModule` is deliberately disjoint from a server-owning
`Application`. The landed foundation composes structured typed routes and maps
actions, context, and authorization values under the root's one action
interpreter; it does not create a child WAI application or a second dispatcher.
Root security, runtime, listener, stores, and request policy remain root-owned.
`inheritApplicationModuleGuards` now appends mapped module guards after every
configured root security phase: a child cannot select
`AuthenticationDisabled`, run before authentication, or replace a parent
restriction. `installApplicationModule` now installs a fully composed module's
route codec, definitions, action metadata/decoder/handler, and inherited
security tail into the existing root `Site`; `buildSiteApplication` remains
the only server application interpreter. `RequestContext` now supplies the
shared safe core (validated origin, locale/fallbacks, correlation, route
observation initialization state, and path prefix), plus a one-way projection
into package-selected identity/client/local views. It deliberately has neither
raw request/proof/header values nor application services. `ActionCodec` now
also has a context-derived prefix adaptation for the root-owned locale
namespace: this keeps local domain action paths mountable without having a
Catalog or Orders package hard-code `/es` or its eventual public URL.

When the module is the complete route table, `applicationModuleSite` creates
that root directly instead of first constructing `apiOnlySite` route fields
that `installApplicationModule` immediately replaces. This keeps both root
adapters available: install for a pre-existing server root with its own route
table, and direct construction for a pluggable composed root. In both cases
there is still exactly one `Site` route/action interpreter and the root keeps
the deployment-owned policies listed above.

The `ActionMount` record exposes only the mappings that the one-way action
decoder/handler consumes: child target/action embedding and parent-action
projection for handler dispatch. An earlier reverse parent-target projection
was removed rather than retained as speculative pluggability: no framework
phase receives a parent target to use it, so a field with that shape would be
a false capability and a source of untestable dead branches. If a later
framework operation genuinely needs parent-target inspection, it must add the
operation and its failure/ownership contract first, then extend this record.

The new Catalog and Orders packages build independently with explicit
query/command adapters, and the composed package uses the same module type at
its outer root. Its allowlisted locale root makes an explicit URL prefix win;
for unprefixed requests a durable preference, cookie, `Accept-Language`, then
the default are selected once before child projection. The route/action tests
prove `/es/catalog`, `/es/orders`, and their localized action declarations;
the child modules only receive bounded locale/scope views. The canonical static
interpreter is now exposed as a typed `StaticAssetRoute` definition and the
composed root mounts it only under `/public/assets/*` (including its localized
form), with `AssetEndpoint` and explicit `AllowUnauthenticated` metadata. It
passes through the existing file-backed WAI response rather than duplicating
traversal, range, conditional, or symlink handling. Early site static assets
remain empty for that example, so the typed endpoint guard boundary owns the
decision.

`staticAssetRouteResponse` is the companion operation for a route already
selected by that typed codec. It exposes only the existing protocol response,
not a second static server; composed definitions use it when page context is
irrelevant, instead of manufacturing a child context solely to call a uniform
page-route callback. `staticAssetRouteDefinition` is layered on the same
operation, keeping the ordinary route-table path and the composed adapter
semantically identical.

### Decision record — AHI-4C: bounded page-security and JWT-claim rails (2026-09-04)

**Decision: preserve the existing one-page-rendering and one-JWT-verification
rails, adding only their missing typed boundary operations.** A route guard
and a protocol route handler now return `NonPageResponse`, the closed subset
of responses which cannot manufacture a page. Only the selected complete SSR
page route receives the freshly prepared `PageSecurity` capability. This
prevents a guard from accidentally rendering a page without a runtime nonce
and CSRF transition, without adding a second dispatcher or a separate page
constructor.

`PageSecurity` deliberately has no public `Eq` instance. A runtime nonce is an
affine framework capability, rather than ordinary application data which may
be compared or reused. The sole framework renderer boundary instead owns
`samePageSecurity`; it compares the opaque values only to confirm that the
handler preserved its renderer-supplied capability. Application code receives
the typed page value it needs to render controls, but no general nonce
comparison/reuse capability.

`CsrfBinding` follows the same rule: applications can provide a binding to the
CSRF capability or persist its separately exposed digest, but cannot compare
bindings as general correlation values. The CSRF interpreter owns its one
private comparison when it evaluates `PageCsrf`; durable adapters compare the
digest they were explicitly given. This avoids widening an otherwise opaque
security proof merely to make record equality convenient.

The account profile page likewise consumes the authenticated
`AccountPrincipal` already attached by the endpoint guard. Its former
cookie/session-store lookup was deleted rather than kept as a second, nominal
fallback rail: it repeated bearer-session interpretation after authentication,
and its absent/expired outcomes were unreachable from a page handler. The
profile store still maps an absent profile to signed-out and retains its typed
unavailable/corrupt error rail. This makes the boundary that establishes a
principal the sole owner of session parsing, expiry, revocation, and
account/session consistency.

The JWT verifier continues to own compact decoding, explicit algorithm
selection, signature validation, and standard-claim validation. Once those
steps succeed, an application claim projection may return only a validated
`SecurityFailureCode`; its code is retained in the ordinary rejected-proof
rail. This allows a durable application to audit distinct malformed account or
session claims while neither raw claims nor the JWT itself reach telemetry,
logs, or public responses. JOSE failures remain Harch's fixed rejection code.
This is an extension of the existing adapter, not a second application JWT
verification path. The reusable framework decision is complete; AHI-4C still
needs its admission module-health follow-up (AHI-4C-AMH) and its remaining
implementation/gate evidence before the task is marked complete.

The application configuration retains the parsed JWT issuer and audience with
their redacted configuration text at startup, rather than reparsing them on
each issuance or verification path. Account IDs follow their own opaque,
ASCII-bounded domain constructor before their JWT subject conversion. This
keeps configuration parsing and subject validity on distinct typed boundaries;
the JWT adapter neither treats an arbitrary request string as a subject nor
requires a second application claim parser.

The former exported `buildRuntimeApp` composition omitted the account JWT
guard and was therefore removed rather than retained as a convenient runtime
test constructor. The only runnable-server composition is now
`buildRuntimeAppWithAccountJwt`, which requires the startup-validated runtime;
tests that need an intentionally unavailable issuer compose the existing
explicit application/workflow builder instead. This closes a capability gap at
the constructor boundary rather than relying on callers to remember which
otherwise-similar runtime constructor is safe to expose.

`ApplicationModule` also carries an immutable construction-owned mount chain.
The root executor attaches it, alongside the declared endpoint/template and
selected locale, to both its security-event sink and the `RequestContext`
before guards run. Thus the composed Catalog observation is
`root/root.catalog/catalog`, not a child-rendered label or request path.
`composed-domains` now proves direct, reload, enhanced, and scripts-disabled
Public/Catalog/Orders navigation in both default and Spanish locale paths; its
browser-only authenticated fixture exercises the protected modules without
claiming that it is an application sign-in flow. Catalog and Orders remain
intentionally protected and their runnable authentication flow is supplied by
AHI-4C.

The composition root's catalog/order mounts, local combination, and localized
action declarations are all fixed literals it owns. It resolves those
declaration-time `Either` results with `requiredModuleConfiguration` instead
of exporting an artificial root error rail: an invalid one is an authored
startup defect, not a user-facing outcome. Its structured error already names
the failing module or endpoint, so the helper accepts only that result rather
than carrying an untyped diagnostic label through a successful construction.
The helper is deliberately narrow; general-purpose module constructors retain
their `Either` results so runtime and caller-provided declarations keep their
typed failure paths.

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
