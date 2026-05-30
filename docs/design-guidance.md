# Design guidance and alignment

This document defines the desired developer experience for spinning up a site on top of the
`harch-web` / `web-api` split, then compares that target to the current repository.

The goal is not to pretend every capability already exists. Instead, this guide keeps two tracks
visible at the same time:

1. **Current-state usage** for features the repository already demonstrates well enough to document.
2. **Target framework direction** for ergonomics and conventions we want the framework to grow into.

## Desired usage model

The ideal "start your own site" flow should look like this:

1. Copy the smallest two-page example.
2. Change a few page/component files without touching infrastructure first.
3. Add features one at a time from isolated example folders: database effects, telemetry, HTTPS,
   middleware/auth, custom API, i18n, reverse proxy, and tests.
4. Graduate to the combined `packages/web-api` app only after the simpler examples make sense.

The first example should stay intentionally small:

- two SSR pages,
- same-origin progressive enhancement for in-app navigation,
- no database,
- no telemetry,
- no HTTPS,
- no reverse proxy,
- a layout component plus page-local styling and browser behavior.

## Direction to standardize on

### 1. Keep the package split

- `packages/harch-web` remains the framework boundary.
- `packages/web-api` remains the default stacked example and composition root.
- The new `examples/` tree becomes the docs-first ladder that shows isolated features.

### 2. Prefer a composition-root story

App startup should read like a composition root that wires together:

- routes,
- pages,
- components,
- middleware,
- effects,
- static assets,
- logging/telemetry,
- listener/security settings.

Today that wiring lives in `packages/web-api/src/WebApi/App.hs` and nearby files. The target shape
should become simpler and more discoverable for new users.

### 3. Move toward folder-driven authoring

The target layout should become more like:

```text
app/Main.hs
src/App/
  App.hs
  Routes.hs
  Pages/
    Home.hs
    Second.hs
  Components/
    Layout.hs
  Api/
    Status.hs
  Effects/
    Database.hs
  Middleware/
    Auth.hs
public/
  app.js
  app.css
```

A custom build-tool-depends defines a common export to configure routes following `Pages/` tree
hierarchy, to then be hooked up by a single line in the composition root, in addition to custom
routes. The framework should own the conventions, generated contract, and helper APIs that make the
result usable. A repo-local preprocessor or Template Haskell generator is an acceptable way to
implement that target, especially because the repo already uses `hspec-discover` and
`spec-preprocessor` as precedent for generated boilerplate.

The target EDSL shape should look closer to the idioms proven by Yesod's route DSL plus the
Shakespearean template family:

- XML-like markup via a custom `harch` template DSL inspired by Hamlet,
- brace-based CSS via Lucius-style templates,
- JavaScript via Julius-style templates,
- type-safe route references in markup, styles, and scripts,
- widget/component composition through normal typed Haskell functions.

Illustrative target shape for `Routes.hs`:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Routes
  ( AppRoute(..)
  , appRoutes
  ) where

import App.Pages.Generated (pagesTreeRoutes)
import App.Api.Status (statusApi)
import HarchWeb

newtype PostSlug = PostSlug Text
  deriving stock (Eq, Show)

data AppRoute
  = HomeR
  | BlogPostR PostSlug
  | StatusApiR
  deriving stock (Eq, Show)

appRoutes :: Routes AppRoute
appRoutes =
  pagesTreeRoutes @AppRoute
    <> [ get "/api/status" StatusApiR statusApi
       ]
```

The important part is the shape rather than the exact names:

- `pagesTreeRoutes` comes from the custom `build-tool-depends`,
- it follows the `Pages/` folder hierarchy,
- it exposes one common export for the composition root to hook up in a single line,
- custom routes still compose on top instead of being blocked by discovery.

Illustrative target shape for `Pages/Home.hs`:

```hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Home
  ( homePage
  ) where

import App.Components.Hero (heroCard, HeroCardProps(..))
import App.Components.Layout (siteLayout, SiteLayoutProps(..))
import App.Routes (AppRoute(..), PostSlug(..))
import HarchWeb

homePage :: Page AppRoute
homePage =
  page HomeR $ \ctx ->
    siteLayout SiteLayoutProps
      { pageTitle = "Home"
      , currentRoute = HomeR
      } $ do
        toWidget
          [harch|
            <section .hero data-page=home>
              <h1>Welcome
              <p>Server-rendered on direct load, progressively enhanced in-app.
              <a .primary-link href=@{BlogPostR (PostSlug "hello-world")}>
                Read the first post
              ^{heroCard HeroCardProps
                  { title = "Second page"
                  , body = "Components stay typed and reusable."
                  , route = BlogPostR (PostSlug "hello-world")
                  }}
          |]
        toWidget
          [lucius|
            .hero {
              display: grid;
              gap: 1rem;
            }

            .primary-link {
              inline-size: fit-content;
            }
          |]
        toWidgetBody
          [julius|
            document.addEventListener("DOMContentLoaded", function () {
              const root = document.querySelector("[data-page='home']");
              if (!root) return;

              root.dataset.homeRoute = "@{HomeR}";
            });
          |]
```

This sample captures the intended direction:

- links are rendered from typed routes like `@{BlogPostR (PostSlug "hello-world")}`,
- path prefixes and similar URL base concerns stay framework-owned through the route renderer,
- components are plain typed functions with explicit input records,
- page-local markup, styles, and client behavior stay colocated and strongly typed.

If a page component needs route inputs, those should be required explicitly in its props rather than
constructed from raw strings inside templates or scripts.

Implementation note:

- app-authored client behavior should move toward the same EDSL-backed story instead of staying in
  separate stringly-typed JavaScript files,
- the existing navigation/runtime JavaScript in `packages/web-api/public/navigation.js` should be
  redesigned to use the same route-aware template/asset pipeline, or another equally typed EDSL
  layer, so route references, prefixes, and shared conventions stay aligned across HTML, CSS, and
  client behavior,
- plain standalone JavaScript should be reserved for lower-level runtime glue only when the typed
  EDSL cannot express the needed behavior cleanly.

### 4. Keep SSR-first progressive enhancement as the default

Hard reloads must keep working for every route. Browser JavaScript should enhance same-origin
navigation and page behaviors, not replace SSR as the primary rendering path.

### 5. Separate what is framework-owned from what is app-owned

The framework should eventually own:

- route-aware link helpers,
- subpath and language-aware URL generation,
- page shell/runtime hooks,
- static asset URL helpers,
- the integration point for custom API routes within the app routing/runtime pipeline,
- the page-discovery contract and helper APIs, while the composition root can opt into generated
  exports from the pages folder,
- default accessibility/security guardrails.

HTTP server dependencies still provide the low-level transport. The framework layer should own how
custom APIs plug into app routing, middleware, observability, security headers, and proxy/subpath
awareness so app code does not have to rebuild that wiring from scratch.

The app should keep owning:

- domain routes and domain page models,
- domain effects and adapters,
- custom middleware and authentication policy,
- custom API handlers and payload shapes,
- branding and app-specific assets.

### 6. Styling direction

Prefer **scoped CSS as the baseline** over a mandatory external utility framework:

- each page or component should be able to define local class names without global collisions,
- explicitly-global styles should still be possible,
- utility classes can remain additive later,
- the smallest example should not require a Node/Tailwind toolchain.

This matches the desired "small SSR-first app" story better than making Tailwind a hard dependency.

### 7. Accessibility direction

Every example should model:

- semantic landmarks and heading structure,
- keyboard-usable navigation and forms,
- visible focus states,
- accessible labels for controls,
- `aria-*` only where native semantics are insufficient,
- a short screen-reader review checklist in the example README.

### 8. Real-time data direction

Automatic server-push updates for database-backed views are desirable, but they should not block the
smallest examples. Treat this as a follow-up framework capability:

- first choice: WebSocket,
- fallback: SSE,
- last resort: long polling.

The desired authoring model is "if a page declares database-backed data, it can opt into live
updates without custom transport code in every app."

## Alignment with the repository today

| Area | Current state | Alignment |
| --- | --- | --- |
| Framework boundary | `packages/harch-web` already exists as the shared SSR/server boundary. | Aligned |
| Combined app/composition root | `packages/web-api` already acts as the real app-owned composition root. | Aligned |
| Two-page SSR example | Home/Second pages already exist in `web-api`. | Aligned |
| Progressive enhancement | `packages/web-api/public/navigation.js` provides same-origin enhancement, but authoring ergonomics are still low-level and link annotation is manual. | Partial |
| Route/path-prefix awareness | Route rendering and forwarded-prefix handling already exist. | Aligned |
| Locale-aware routing | Locale is represented today, but a beginner-facing path-based i18n story is not yet documented as an example. | Partial |
| PostgreSQL-backed effects | The repo already demonstrates PostgreSQL-backed behavior and migration wiring. | Aligned |
| Alternative database adapters | The effect seam exists conceptually, but a polished adapter example is not yet documented. | Partial |
| Logging and telemetry | OTLP tracing and related docs already exist. | Aligned |
| HTTPS/manual certs/shared certs/ACME | Already supported and documented. | Aligned |
| HSTS/redirect/security headers | Already supported and documented. | Aligned |
| Reverse proxy awareness | Already supported, documented, and backed by nginx examples. | Aligned |
| Testing story | Unit, integration, E2E, and coverage workflows already exist. | Aligned |
| Middleware/auth/JWT example | Not yet exposed as a polished app-authoring example. | Missing |
| Custom additional API example | Possible in the current shape, but not documented as an isolated example. | Partial |
| Page/component file ergonomics | Current app uses lower-level Haskell modules rather than a cleaner page/component convention. | Missing |
| Folder discovery and route generation | Not implemented yet. | Missing |
| Scoped CSS support | Not implemented yet. | Missing |
| Real-time subscriptions | Not implemented yet. | Missing |
| Accessibility guidance | Some semantics exist, but there is no repo-level accessibility guidance/example standard. | Partial |

## Example strategy

The `examples/` tree should stay beginner-first and feature-isolated.

Use these status labels:

- **Current**: can be shown using the repository as it exists now.
- **Iterative**: should be documented now, but the example should call out rough edges that later
  framework work should smooth over.
- **Aspirational**: document the desired file shape and workflow, then track framework work needed to
  make it real.

The example ladder should be:

1. two-page SSR + progressive enhancement,
2. PostgreSQL effects,
3. custom database adapter,
4. logging + telemetry,
5. testing,
6. HTTPS with provided certificate,
7. HTTPS with mkcert/self-signed local development,
8. HTTPS with ACME,
9. HTTPS security/HSTS/redirects,
10. middleware + auth + JWT cookie,
11. custom JavaScript behaviors,
12. custom additional API,
13. path-based multi-language routing,
14. reverse-proxy awareness.

## Repository files that anchor the current shape

- `README.md`
- `SETUP.md`
- `packages/harch-web/src/HarchWeb.hs`
- `packages/web-api/src/WebApi/App.hs`
- `packages/web-api/src/WebApi/Route.hs`
- `packages/web-api/src/WebApi/Page.hs`
- `packages/web-api/src/WebApi/App/Shell.hs`
- `packages/web-api/src/WebApi/App/Enhancements.hs`
- `packages/web-api/public/navigation.js`
- `examples/runtime-config/*`
- `examples/reverse-proxy/*`

## Framework follow-ups the docs should keep visible

These should stay explicit instead of being hidden inside prose:

1. page and component discovery from folder structure,
2. route override/custom route support on top of discovery,
3. cleaner composition-root helpers,
4. scoped CSS support with optional global escape hatches,
5. middleware/auth ergonomics including JWT cookies,
6. a custom API story separate from the CRUD-style app flow,
7. path-based i18n defaults such as `/path` meaning `/en/path`,
8. live data subscriptions for database-backed pages,
9. accessibility helper patterns and review guidance.

## What to adhere to right now

Until the deeper framework redesign lands, new docs/examples in this repo should follow these rules:

1. Start with the smallest SSR-first example, not the most feature-rich one.
2. Keep examples isolated instead of stacking multiple concerns into a single walkthrough.
3. Distinguish current behavior from target behavior plainly.
4. Reuse existing runtime/security/testing capabilities where they already work.
5. Treat page discovery, scoped CSS, and live subscriptions as tracked design follow-ups, not as
   hidden assumptions.
