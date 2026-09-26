# src/App/Api/Status.hs

```hs
module App.Api.Status (statusRouteDefinition) where

import HarchWeb
  ( Response (BodyResponse)
  , ResponseBody (..)
  , RouteRequest
  )
import HarchWeb.Site (RouteDefinition (..))

statusRouteDefinition :: RouteDefinition AppRoute AppRequestContext
statusRouteDefinition =
  RouteDefinition
    { routeNavigationLabel = Nothing
    , routeResponse = statusResponse
    }

statusResponse :: RouteRequest AppRoute AppRequestContext -> IO (Response AppRoute AppRequestContext)
statusResponse _ =
  pure
    ( BodyResponse
        ResponseBody
          { responseStatus = 200
          , responseContentType = "application/json; charset=utf-8"
          , responseBody = "{\"status\":\"ok\"}"
          , responseObservabilityAttributes = []
          , responseLogEntries = []
          }
    )
```

Add `StatusApi` to the app-owned `ApiRoute` ADT and its parser/renderer, then return
`statusRouteDefinition` from the `Api StatusApi` branch of the total application dispatcher. This keeps
custom APIs typed without asking the framework to guess a CRUD model.
