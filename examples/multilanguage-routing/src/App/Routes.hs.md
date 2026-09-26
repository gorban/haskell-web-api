# src/App/Routes.hs

```hs
module App.Routes where

data Language = En | Es

languageAwareRoutes :: [Route]
languageAwareRoutes =
  [ localized En "/about"
  , localized Es "/about"
  ]
```

The framework should eventually own the boring parts here: default-language behavior, path prefixing,
and language-aware link generation.
