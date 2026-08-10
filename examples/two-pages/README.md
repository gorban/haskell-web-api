# two-pages

**Status:** Runnable

This is the smallest application to copy first. It demonstrates:

- complete SSR documents on direct loads and reloads,
- generated, exhaustive page routing plus explicit API and dynamic routes,
- XML-like typed components and scoped CSS,
- same-origin enhanced navigation with native-link fallback,
- immediate form capture, input preservation, typed region patches,
- an SSR-first live-data page with deferred SSE enhancement, and
- a CSRF-protected native multipart file-upload form dispatched through `HarchWeb.Api`, composed in
  front of the site via `runServerWithWaiMiddleware`.

It deliberately has no database, telemetry collector, HTTPS, or reverse-proxy prerequisite.

Run it from the repository root:

```sh
cabal run two-pages-example
```

Then visit:

1. <http://127.0.0.1:8080/>
2. <http://127.0.0.1:8080/second>
3. <http://127.0.0.1:8080/live-data>
4. <http://127.0.0.1:8080/preview/example>
5. <http://127.0.0.1:8080/native-upload>

The executable composition is in [App.App](src/App/App.hs), with route parsing in
[App.Routes](src/App/Routes.hs) and the shared page shell in
[App.Components.Layout](src/App/Components/Layout.hs).

## Generated page routes

The Cabal setup hook discovers modules below `src/App/Pages/` and runs the `harch-page-routes` build
tool. It generates a closed `PageRoute`, its path codec, and its exhaustive definition dispatcher:

```hs
data PageRoute
  = HomePage
  | LiveDataPage
  | PageNotFound
  | SecondPage
  deriving (Bounded, Enum, Eq, Show)

allPageRoutes :: [PageRoute]
allPageRoutes = [minBound .. maxBound]

pageRouteDefinition :: PageRoute -> RouteDefinition TwoPageRoute ()
pageRouteDefinition route =
  case route of
    HomePage -> App.Pages.Home.pageDefinition
    LiveDataPage -> App.Pages.LiveData.pageDefinition
    PageNotFound -> App.Pages.NotFound.pageDefinition
    SecondPage -> App.Pages.Second.pageDefinition
```

Each discovered module exports `pageDefinition`, so adding a page cannot leave a missing runtime
registration. The generated mapping is:

| Page module | Constructor | Path | Navigation label |
| --- | --- | --- | --- |
| `App.Pages.Home` | `HomePage` | `/` | `Home` |
| `App.Pages.LiveData` | `LiveDataPage` | `/live-data` | `Live data` |
| `App.Pages.NotFound` | `PageNotFound` | `/404` | none |
| `App.Pages.Second` | `SecondPage` | `/second` | `Second` |

The application owns the route-family sum:

```hs
data TwoPageRoute
  = Page PageRoute
  | Api ApiRoute
  | Custom CustomRoute
  deriving (Eq, Show)
```

Generated pages dispatch totally through `pageRouteDefinition`. `/live-data/events` is an explicit
typed API route, while `/preview/:slug` demonstrates an explicit dynamic path whose slug is validated
into `PreviewSlug`. The route family determines response capability; there is no separate
`RequestSurface` value that could disagree with it.

## Typed component forms

[App.Pages.Home](src/App/Pages/Home.hs) uses every supported component-call form in rendered content:

```hs
-- Nullary record props and no children.
<SubscriptionEmailField />

-- Named record fields and a nested HTML child.
<AuthorCard authorName="Harch Web team" authorRole="SSR framework maintainers">
  <p>The page and its controls are complete before optional JavaScript loads.</p>
</AuthorCard>

-- Two distinct positional arguments and a nested HTML child.
<AuthorAvatar props={[AuthorIdentity "HW", CompactAvatar]}>
  <p>Maintained as a small, runnable framework reference.</p>
</AuthorAvatar>
```

Braced expressions provide named values from ordinary Haskell. `App.Pages.Home` uses the compiled
top-level `aboutAuthorName` and `aboutAuthorRole` values:

```hs
<AuthorCard authorName={aboutAuthorName} authorRole={aboutAuthorRole}>
  <p>The page and its controls are complete before optional JavaScript loads.</p>
</AuthorCard>
```

`children={computedChildren}` supplies a computed `[Html]` instead of nested markup. The compiled
quasiquoter test uses that form directly:

```hs
let computedChildren = [element paragraphTag [] [text "Computed child"]]
    computedChildrenQuoted =
      [harch|<Account.HeroCard heroTitle="Second page" children={computedChildren} />|]
```

Prefer named fields for cohesive record props. Use the positional `props` list only when a component's
ordinary Haskell function intentionally takes multiple distinct typed inputs, as `AuthorAvatar` does.
The quasiquoter lowers all forms to record construction or normal function application and then builds
the same escaping-by-default `Html` AST.

`SubscriptionEmailField` also shows that a component body can use the same quasiquoter as a page:

```hs
subscriptionEmailField SubscriptionEmailFieldProps children =
  [harch|
    <label for="subscription-email">Email address</label>
    <input id="subscription-email" name="email" type="email"
           autocomplete="email" required />
    {children}
  |]
```

## Behavior with and without deferred modules

1. Direct loads and reloads of each page return complete HTML.
2. Annotated same-origin links upgrade to fetch/history navigation after the deferred module loads.
3. Back and Forward retain enhanced navigation behavior.
4. With scripts disabled, native links still perform ordinary document navigation.
5. The subscription form can be filled and submitted while `navigation.js` is blocked. The inline
   kernel preserves its values without navigation, and the deferred module later drains the action and
   applies the returned `RegionPatch`.
6. `/live-data` begins with meaningful SSR status. Its deferred `EventSource` module replaces that
   status after an event; without scripts, the initial content remains.

The subscription target is declared once in `App.Routes.twoPageActions` as an
`ActionCodec TwoPageActionTarget () TwoPageAction`. `App.Components.Controls.actionForm` passes that
codec and a typed `Subscribe` target to `HarchWeb.Controls.actionForm`; the same declaration decodes the
captured request in `App.App`. The focused unit test proves the `POST` subscription path, unknown-path
rejection, method mismatch, and duplicate-field parse outcome. Framework transport tests cover the
corresponding safe `404`, `405` with `Allow`, and `400` responses; a decoded invalid subscription remains
the example's localized `422` region patch.

The subscription form also renders a control-local accessible recovery region. The inline kernel captures
the submission and input snapshot before `/assets/navigation.js` is available, marks that same form pending,
and retains its envelope until the deferred handler completes, reports a recoverable outcome, or the user
cancels. A delayed module is not considered failed merely because time passes; the visible state can change
to delayed without retrying or submitting. This example uses the default exclusive-client capability, so
scripts-disabled native submission is intentionally not the fallback for this client action.

The lifecycle suite also demonstrates the two explicit retry contracts. `HandlerSafeRetry` exposes a
control-local retry only after a recoverable handler result and reuses the captured values. An
`IdempotentMutationRetry` retains its `ActionIdempotency` key across attempts and forwards it to the typed
server action request; an application must use that value at its durable deduplication boundary. Neither
contract causes an automatic retry.

The adjacent “Native fallback subscription” form demonstrates the opt-in alternative. It supplies a
server-owned fallback endpoint and CSRF form value; with scripts disabled it posts to a complete SSR
confirmation page only when the matching CSRF cookie is present. Its enhanced path continues to use the
typed `/actions/subscribe` codec endpoint.

## CSRF-protected native file upload

`/native-upload` ([App.NativeUpload](src/App/NativeUpload.hs)) is a plain
`<form enctype="multipart/form-data">` with no `data-harch-action` attribute, so the inline capture
kernel's `form[data-harch-action="true"]` selector never matches it — no file bytes are ever read by
client script, with or without JavaScript. It is dispatched through `HarchWeb.Api.apiEndpointMiddleware`
rather than through `App.Routes.routeCodec`, since a native `POST` needs the raw, incremental request
body that path-only route matching does not have; `App.App.buildNativeUploadMiddleware` composes that
middleware in front of the site's own application via `HarchWeb.runServerWithWaiMiddleware`
(production, in [app/Main.hs](app/Main.hs)) or `HarchWeb.withLocalTestServerForApplication` (tests, in
[test/E2E/AppSpec.hs](test/E2E/AppSpec.hs)).

CSRF policy: the form carries a single-use, server-held token (embedded as a hidden field, generated on
every `GET`) rather than a double-submit cookie, since no framework change is needed to let a plain page
response set a cookie header this way. `withMultipartRequestBodyWith`'s per-part callback validates
that field as soon as it finishes — before any later part, including the file part, is read — so a
request whose file part precedes an invalid or absent CSRF field is rejected before that file is retained
in the bounded in-memory adapter; the CSRF field must precede the file field in the form markup for the common,
well-formed case to get this benefit, not merely a rejected response after the fact. Accepted uploads are
explicitly discarded because this example has no durable ownership requirement; production handlers must
instead deliberately promote uploads through their selected storage adapter. See the module haddock for
the full policy and its explicitly accepted limitations (single outstanding token, no expiry).

## Verification

Unit tests cover generated routes, dispatch, component output, actions, patches, SSE, and configuration:

```sh
cabal test two-pages-example-tests --test-show-details=direct --test-options='--skip E2E'
```

After installing the locked Playwright dependency and Chromium described in
[SETUP.md](../../SETUP.md), run the real-browser scenarios:

```sh
cabal test two-pages-example-tests \
  --test-show-details=direct \
  --test-options='--match real-browser'
```

The [E2E source](test/E2E/AppSpec.hs) verifies enhanced navigation, Back/Forward, default
scripts-disabled non-submission for exclusive client actions, early-submit preservation, delayed handler
arrival (including after the liveness threshold), cancellation before late registration, throwing,
rejected, and never-settling handlers, deferred-script failure, stale settlement rejection, multiple
pending controls, conditional leave warning, eventual region patches, the SSE update using semantic
locators and composed retrying observations, and the native multipart upload flow (both with scripts
enabled and disabled) completing as a hard navigation with zero capture-kernel mutation requests.

## Source map

- [SetupHooks.hs](SetupHooks.hs): page discovery and generated-module build wiring.
- [App.App](src/App/App.hs): site composition, total route dispatch, actions, regions, and server config.
- [App.Routes](src/App/Routes.hs): page/API/custom route sum, parsing, rendering, dynamic slug type, and
  the shared subscription `ActionCodec`.
- [App.Components.Controls](src/App/Components/Controls.hs): page-only navigation links and the typed
  `ActionForm` wrapper that prints its target and method from `twoPageActions`.
- [App.Pages.Home](src/App/Pages/Home.hs): component forms and captured subscription control.
- [App.CustomPages.Preview](src/App/CustomPages/Preview.hs): explicit dynamic page route.
- [App.NativeUpload](src/App/NativeUpload.hs): CSRF-protected native multipart file-upload form,
  dispatched through `HarchWeb.Api.apiEndpointMiddleware` rather than `App.Routes.routeCodec`.
- [HarchWeb.Document](../../packages/harch-web/src/HarchWeb/Document.hs): embedded deferred
  navigation/action runtime served at `/assets/navigation.js`.
- [public/live-data.js](public/live-data.js): page-scoped SSE enhancement.
- [Unit tests](test/Unit/AppSpec.hs) and [real-browser tests](test/E2E/AppSpec.hs).
