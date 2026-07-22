# src/App/Api/Status.hs

```hs
module App.Api.Status (statusRoute) where

import HarchWeb
  ( Response (BodyResponse)
  , ResponseBody (..)
  , RouteRequest
  )
import HarchWeb.Site (SiteRoute (..))

statusRoute :: SiteRoute AppRoute AppRequestContext
statusRoute =
  SiteRoute
    { siteRouteValue = StatusApiRoute
    , siteRouteNavigationLabel = Nothing
    , siteRouteResponse = statusResponse
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

Add `StatusApiRoute` to the app-owned route ADT and its parser/renderer, then include `statusRoute`
in the list passed to `buildSiteApplication`. This keeps custom APIs typed without asking the
framework to guess a CRUD model.
