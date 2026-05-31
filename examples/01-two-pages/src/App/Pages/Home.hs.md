# src/App/Pages/Home.hs

Desired page-authoring shape:

```hs
module App.Pages.Home where

import App.Routes (Route (..))
import Harch.Page

homePage :: Page Route
homePage =
  page HomeRoute do
    title "Home"
    styles do
      lucius do
        ".page-summary { max-width: 40rem; }"
    body do
      section_ [class_ "page-summary", data_ "page" "home"] do
        h1_ "Home"
        p_ "This page is fully server-rendered on direct load and reload."
        p_ do
          routeLink_ SecondRoute "Go to the second page"
```

The generated Haskell should preserve hard-reload equivalence: opening `/` directly and navigating
there from another page should produce equivalent page content. Route links should compile through
the typed route codec, and HarchWeb should add any progressive-navigation annotations required by
the shared runtime.

Page-local `styles` and `client` blocks should attach to the typed page output, then flow through
the shared shell as scoped `<style>` and deferred script content. The page module owns local
presentation and behavior; the layout decides where collected page assets are rendered.
