# src/App/Pages/Second.hs

```hs
module App.Pages.Second where

secondPage :: Page
secondPage =
  page "/second"
    { title = "Second"
    , body =
        section_ $ do
          h2_ "Second page"
          p_ "This route should also work with JavaScript disabled."
          pageLink homeRoute "Back home"
    }
```

Both pages should stay valid SSR endpoints. Progressive enhancement is an optimization, not a
separate rendering mode.
