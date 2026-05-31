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
    body do
      section_ [data_ "page" "second"] do
        h1_ "Second"
        p_ "This page also returns full HTML when loaded directly."
        p_ do
          routeLink_ HomeRoute "Back home"
```

Both pages should stay valid SSR endpoints. Progressive enhancement is an optimization layered on
the same route and page definitions, not a separate rendering mode.
