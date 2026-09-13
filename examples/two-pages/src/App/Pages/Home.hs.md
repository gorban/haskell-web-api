# src/App/Pages/Home.hs

Desired page-authoring shape:

```hs
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Home where

import App.Routes (Route (..))
import Harch.Page

homePage :: Page Route
homePage =
  page HomeRoute do
    title "Home"

    markup [harch|
      <section class=#{home.summary} data-page="home">
        <h1>Home</h1>
        <p>This page is fully server-rendered on direct load and reload.</p>
        <p>
          <a href=@{SecondRoute} class=#{home.primaryLink}>
            Go to the second page
          </a>
        </p>
      </section>
    |]

    styles [lucius|
      .#{home.summary} {
        max-width: 40rem;
      }

      .#{home.primaryLink} {
        font-weight: 600;
      }
    |]

    client [julius|
      const root = document.querySelector(".#{home.summary}");

      if (root) {
        root.dataset.homeReady = "true";
      }
    |]
```

The generated Haskell should preserve hard-reload equivalence: opening `/` directly and navigating
there from another page should produce equivalent page content. Route links should compile through
the typed route codec, and HarchWeb should add any progressive-navigation annotations required by
the shared runtime.

The target should feel closer to a single-file component than a loose set of builders:

- `harch`, `lucius`, and `julius` blocks are real quasiquoted syntax, not strings hidden inside
  builder calls.
- `@{SecondRoute}` is route-typed URL interpolation, so links use the route codec and keep prefix
  handling framework-owned.
- `#{home.summary}` and `#{home.primaryLink}` are page-scoped style slots. In markup they render as
  class names; in Lucius/Julius selector positions they interpolate into the corresponding scoped
  selectors, so CSS and JavaScript can target the same generated classes without repeating string
  literals.
- Explicit global styling should be opt-in, for example through a `global` or `unsafeGlobal`
  wrapper, rather than the default for page/component styles.

Page-local `styles` and `client` blocks should attach to the typed page output, then flow through
the shared shell as scoped `<style>` and deferred script content. The page module owns local
presentation and behavior; the layout decides where collected page assets are rendered.
