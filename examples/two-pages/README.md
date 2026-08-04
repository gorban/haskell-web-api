# two-pages

**Status:** Runnable

This is the smallest application to copy first. It demonstrates:

- complete SSR documents on direct loads and reloads,
- generated, exhaustive page routing plus explicit API and dynamic routes,
- XML-like typed components and scoped CSS,
- same-origin enhanced navigation with native-link fallback,
- immediate form capture, input preservation, typed region patches, and
- an SSR-first live-data page with deferred SSE enhancement.

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
arrival, a permanently blocked action becoming visibly recoverable until cancellation, eventual region
patches, and the SSE update using semantic locators and composed retrying observations.

## Source map

- [SetupHooks.hs](SetupHooks.hs): page discovery and generated-module build wiring.
- [App.App](src/App/App.hs): site composition, total route dispatch, actions, regions, and server config.
- [App.Routes](src/App/Routes.hs): page/API/custom route sum, parsing, rendering, dynamic slug type, and
  the shared subscription `ActionCodec`.
- [App.Components.Controls](src/App/Components/Controls.hs): page-only navigation links and the typed
  `ActionForm` wrapper that prints its target and method from `twoPageActions`.
- [App.Pages.Home](src/App/Pages/Home.hs): component forms and captured subscription control.
- [App.CustomPages.Preview](src/App/CustomPages/Preview.hs): explicit dynamic page route.
- [HarchWeb.Document](../../packages/harch-web/src/HarchWeb/Document.hs): embedded deferred
  navigation/action runtime served at `/assets/navigation.js`.
- [public/live-data.js](public/live-data.js): page-scoped SSE enhancement.
- [Unit tests](test/Unit/AppSpec.hs) and [real-browser tests](test/E2E/AppSpec.hs).
