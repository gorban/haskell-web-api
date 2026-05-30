# 01-two-pages

**Status:** Iterative

Start here. This is the smallest example we want new users to copy first:

- two pages,
- SSR for direct loads and reloads,
- progressive enhancement for same-origin navigation,
- a shared layout component,
- no database, telemetry, HTTPS, or reverse proxy yet.

Current repo alignment:

- `packages/web-api/src/WebApi/Page.hs` already models a two-page app.
- `packages/web-api/public/navigation.js` already enhances same-origin navigation.
- The desired file layout below is cleaner than the current example-app wiring, so treat it as the
  target shape rather than today's exact API.

Suggested files:

- [app/Main.hs.md](app/Main.hs.md)
- [src/App/Components/Layout.hs.md](src/App/Components/Layout.hs.md)
- [src/App/Pages/Home.hs.md](src/App/Pages/Home.hs.md)
- [src/App/Pages/Second.hs.md](src/App/Pages/Second.hs.md)
- [public/app.js.md](public/app.js.md)

## Starter shape

The intended first composition root is deliberately tiny:

```hs
main :: IO ()
main =
  runSite $
    site
      { appName = "my-site"
      , pages = [homePage, secondPage]
      , layout = siteLayout
      , staticAssets = "public"
      }
```

The point is not the exact API surface yet. The point is the workflow:

- define pages as server-rendered routes,
- share a small layout component,
- ship a tiny browser enhancement layer,
- keep the app understandable before adding any effects or deployment concerns.

## Expected behavior

1. Loading `/` directly returns complete HTML rendered on the server.
2. Loading `/second` directly also returns complete HTML rendered on the server.
3. Clicking an in-app same-origin link upgrades to enhanced navigation rather than forcing a full
   browser reload.
4. Using Back and Forward keeps the app navigable.
5. Disabling JavaScript still leaves both pages fully usable through normal links.

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
