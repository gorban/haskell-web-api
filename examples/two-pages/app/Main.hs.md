# app/Main.hs

```hs
module Main where

import App.Components.Layout (siteLayout)
import App.Pages.Home (homePage)
import App.Pages.Second (secondPage)

main :: IO ()
main =
  runSite $
    site
      { appName = "my-site"
      , pages = [homePage, secondPage]
      , layout = siteLayout
      , staticAssets = "public"
      }
```

This is the desired composition-root feel: one obvious place to wire pages, layout, and assets.
Today the closest real-world equivalent lives in `packages/web-api/src/WebApi/App.hs`.
