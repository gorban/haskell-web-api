# two-pages

**Status:** Working

Start here. This is the smallest example we want new users to copy first:

- two pages,
- SSR for direct loads and reloads,
- progressive enhancement for same-origin navigation,
- a shared layout component,
- no database, telemetry, HTTPS, or reverse proxy yet.

This example now builds with the current framework seam:

- [two-pages-example.cabal](two-pages-example.cabal)
- [app/Main.hs](app/Main.hs)
- [src/App/App.hs](src/App/App.hs)
- [src/App/Routes.hs](src/App/Routes.hs)
- [src/App/Components/Layout.hs](src/App/Components/Layout.hs)
- [src/App/Pages/Home.hs](src/App/Pages/Home.hs)
- [src/App/Pages/Second.hs](src/App/Pages/Second.hs)
- [public/navigation.js](public/navigation.js)
- [test/Unit/AppSpec.hs](test/Unit/AppSpec.hs)
- [test/E2E/AppSpec.hs](test/E2E/AppSpec.hs)

Run it from the repository root with:

```bash
cabal run two-pages-example
```

Then visit:

1. `http://127.0.0.1:8080/`
2. `http://127.0.0.1:8080/second`

The current composition root is still lower-level than the long-term `harch` / page-discovery goal,
but it already shows the intended workflow:

- define typed routes,
- wire pages through `HarchWeb.Site`,
- share a small layout component,
- ship a tiny browser enhancement layer at `/assets/navigation.js`,
- keep the app understandable before adding any effects or deployment concerns.

## Desired generated page surface

The polished version should let the composition root opt into discovered pages with one generated
export and keep explicit custom routes alongside it:

```hs
twoPageSite =
  ( simpleSite
      "two-pages-example"
      ()
      routeCodec
      twoPageShell
      (pagesTreeRoutes <> customRoutes)
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy
    }

customRoutes =
  [ pageSiteRoute NotFoundRoute Nothing notFoundPage
  ]
```

The custom build tool should generate `pagesTreeRoutes` from `src/App/Pages/**/*.hs`, using the
same `SiteRoute` shape the example already wires by hand today:

```hs
pagesTreeRoutes :: [SiteRoute TwoPageRoute ()]
pagesTreeRoutes =
  [ pageSiteRoute HomeRoute (Just "Home") homePage,
    pageSiteRoute SecondRoute (Just "Second") secondPage
  ]
```

For this example, the first hierarchy mapping is intentionally small:

| Page module | Route constructor | Path | Navigation label |
| --- | --- | --- | --- |
| `App.Pages.Home` | `HomeRoute` | `/` | `Home` |
| `App.Pages.Second` | `SecondRoute` | `/second` | `Second` |

That keeps the first example focused on SSR, progressive enhancement, typed routes, shared layout,
generated page routes, and custom-route composition. Database access, auth, i18n, telemetry,
deployment concerns, and typed page-local asset generation are intentionally deferred to later
examples.

## Expected behavior

1. Loading `/` directly returns complete HTML rendered on the server.
2. Loading `/second` directly also returns complete HTML rendered on the server.
3. Clicking an in-app same-origin link upgrades to enhanced navigation rather than forcing a full
   browser reload.
4. Using Back and Forward keeps the app navigable.
5. Disabling JavaScript still leaves both pages fully usable through normal links.

## Real-browser tests

Install the locked Playwright dependency and Chromium as described in the repository `SETUP.md`, then
run the Haskell-authored browser scenarios with:

```bash
cabal test two-pages-example-tests --test-show-details=direct --test-options='--match real-browser'
```

Prefer semantic locators and callback assertions. CSS remains an explicit structural escape hatch:

```hs
runBrowserScenario browser $ do
  visit homeUrl
  assertText (byRole Heading) (`shouldBe` "Home")
  click (byRole Link `named` "Go to the second page")
  assertMetrics $ \metrics ->
    $([|metrics|] `shouldMatch`
      [p|BrowserMetrics
           { enhancedNavigationFetchCount = 1
           , hardNavigationCount = 0
           }|])
```

Potentially expensive properties stay demand-driven. Compose only the observations needed by a test;
the harness batches those leaves into one browser request:

```hs
data FieldState = FieldState Text Bool

fieldState target =
  FieldState <$> inputValue target <*> isFocused target
```

## What this example is trying to standardize

- SSR stays the primary rendering path.
- Progressive enhancement is additive rather than required.
- The smallest app should not require database setup, tracing infrastructure, TLS, or a reverse
  proxy before someone can understand the framework shape.

## What to explain in the final polished example

1. direct loads of `/` and `/second` render full HTML on the server,
2. clicking an in-app same-origin link upgrades to a fetch/replace flow,
3. Back/Forward still works,
4. disabling JavaScript still leaves both pages usable.
