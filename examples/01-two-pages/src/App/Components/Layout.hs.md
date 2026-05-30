# src/App/Components/Layout.hs

```hs
module App.Components.Layout where

siteLayout :: Html -> Html
siteLayout bodyContent =
  htmlDocument "my-site" $ do
    header_ $ do
      h1_ "my-site"
      nav_ $ do
        pageLink homeRoute "Home"
        pageLink secondRoute "Second"
    main_ bodyContent
    script_ [src_ "/assets/app.js", defer_ "true"] ""
```

The important part is not the exact DSL surface. The point is that layout stays a reusable
component, while route-aware link helpers stay framework-owned.
