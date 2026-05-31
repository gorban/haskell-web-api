# src/App/Pages/Second.hs

Desired page-authoring shape:

```hs
module App.Pages.Second where

import App.Routes (Route (..))
import Harch.Page

secondPage :: Page Route
secondPage =
  page SecondRoute do
    title "Second"
    bootstrapHook "second-page"
    styles do
      lucius do
        ".second-page-note { font-weight: 600; }"
    client do
      julius do
        "document.documentElement.dataset.secondPageReady = 'true';"
    body do
      section_ [data_ "page" "second"] do
        h1_ "Second"
        p_ [class_ "second-page-note"] "This page also returns full HTML when loaded directly."
        p_ do
          routeLink_ HomeRoute "Back home"
```

Both pages should stay valid SSR endpoints. Progressive enhancement is an optimization layered on
the same route and page definitions, not a separate rendering mode. Page-local `styles` and
`client` blocks should be rendered into the same SSR document, so enhanced navigation can reuse the
page definition instead of inventing a second asset path for the SPA case.
