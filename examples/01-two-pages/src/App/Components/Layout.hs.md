# src/App/Components/Layout.hs

Desired layout/component shape:

```hs
module App.Components.Layout where

import App.Routes (Route (..))
import Harch.Layout

twoPageLayout :: Layout Route
twoPageLayout =
  layout do
    bodyAttr "data-app" "two-pages-example"
    nav do
      routeLink_ HomeRoute "Home"
      routeLink_ SecondRoute "Second"
    main_ [id_ "app-main"] pageBody
```

The important part is not the exact module names. The direction is that layout stays a reusable
component, navigation remains typed, and HarchWeb owns framework conventions such as active links,
navigation shell markers, and the shared same-origin navigation runtime.
