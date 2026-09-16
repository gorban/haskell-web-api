# src/App/Components/Layout.hs

Desired layout/component shape:

```hs
{-# LANGUAGE QuasiQuotes #-}

module App.Components.Layout where

import App.Routes (Route (..))
import Harch.Layout

twoPageLayout :: Layout Route
twoPageLayout =
  layout do
    shell [harch|
      <body data-app="two-pages-example" class=#{layout.body}>
        <nav class=#{layout.nav} aria-label="Primary">
          <a href=@{HomeRoute} class=#{layout.navLink}>Home</a>
          <a href=@{SecondRoute} class=#{layout.navLink}>Second</a>
        </nav>

        <PageStyles />

        <main id="app-main" class=#{layout.main}>
          <PageBody />
        </main>

        <PageScripts />
      </body>
    |]

    styles [lucius|
      .#{layout.body} {
        min-height: 100vh;
      }

      .#{layout.nav} {
        display: flex;
        gap: 1rem;
      }

      .#{layout.navLink}[aria-current="page"] {
        font-weight: 700;
      }

      .#{layout.main} {
        padding-block: 2rem;
      }
    |]
```

The important part is not the exact module names. The direction is that layout stays a reusable
component, navigation remains typed, and HarchWeb owns framework conventions such as active links,
navigation shell markers, and the shared same-origin navigation runtime.

The layout target should use the same single-file component style as pages:

- `shell [harch| ... |]` defines the document frame with familiar XML-like markup.
- `href=@{HomeRoute}` and `href=@{SecondRoute}` keep navigation route-typed and prefix-safe.
- `#{layout.body}`, `#{layout.nav}`, `#{layout.navLink}`, and `#{layout.main}` are layout-scoped
  style slots shared by markup and Lucius without handwritten class strings.
- `<PageStyles />`, `<PageBody />`, and `<PageScripts />` are framework-owned placeholders for the
  current page's colocated Lucius, markup, and Julius output.
- HarchWeb should still add active-link attributes, navigation shell markers, and the shared
  same-origin navigation runtime automatically.

Page-local Lucius and Julius attach to the page result and are rendered by layout placeholders,
keeping the page readable without requiring hand-written `<style>` or `<script>` plumbing in every
module.
