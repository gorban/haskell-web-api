# src/App/Api/Status.hs

```hs
module App.Api.Status where

statusRoute :: ApiRoute
statusRoute =
  get "/api/status" $ do
    json
      [ "ok" .= True
      , "service" .= ("my-site" :: Text)
      ]
```

The design guidance should keep custom API handlers app-owned rather than hiding them inside generic
framework CRUD flows.
