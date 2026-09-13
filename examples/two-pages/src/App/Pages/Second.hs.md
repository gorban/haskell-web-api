# src/App/Pages/Second.hs

Desired page-authoring shape:

```hs
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Second where

import App.Routes (Route (..))
import Harch.Page

secondPage :: Page Route
secondPage =
  page SecondRoute do
    title "Second"
    bootstrapHook "second-page"

    markup [harch|
      <section class=#{second.summary} data-page="second">
        <h1>Second</h1>
        <p class=#{second.note}>
          This page also returns full HTML when loaded directly.
        </p>
        <p>
          <a href=@{HomeRoute}>Back home</a>
        </p>
      </section>
    |]

    styles [lucius|
      .#{second.summary} {
        max-width: 40rem;
      }

      .#{second.note} {
        font-weight: 600;
      }
    |]

    client [julius|
      document.documentElement.dataset.secondPageReady = "true";
    |]
```

Both pages should stay valid SSR endpoints. Progressive enhancement is an optimization layered on
the same route and page definitions, not a separate rendering mode. Page-local `styles` and
`client` blocks should be rendered into the same SSR document, so enhanced navigation can reuse the
page definition instead of inventing a second asset path for the SPA case.
