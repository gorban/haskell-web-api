module WebApi.Response
  ( renderApiResponseFromRouteData,
    selectResponseWithDatabase,
    selectResponse,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Config (AppConfig)
import WebApi.Database (DatabaseEffect, defaultDatabaseEffect)
import WebApi.Page (renderPageFromRouteData)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute,
    RequestSurface (..),
  )
import WebApi.RouteData
  ( RouteDataResult (..),
    SecondRouteData (..),
    StatusApiData (..),
    selectRouteDataWithDatabase,
  )

selectResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectResponse config =
  selectResponseWithDatabase config defaultDatabaseEffect

selectResponseWithDatabase :: AppConfig -> DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectResponseWithDatabase config databaseEffect routeRequest =
  fmap
    ( \routeData ->
        case requestSurface (HarchWeb.requestContext routeRequest) of
          ApiSurface ->
            HarchWeb.BodyResponse (renderApiResponseFromRouteData routeData)
          PageSurface ->
            HarchWeb.PageResponse (renderPageFromRouteData config routeRequest routeData)
    )
    (selectRouteDataWithDatabase databaseEffect routeRequest)

renderApiResponseFromRouteData :: RouteDataResult -> HarchWeb.ResponseBody
renderApiResponseFromRouteData routeData =
  case routeData of
    StatusApiDataResult statusApiData ->
      HarchWeb.ResponseBody
        { HarchWeb.responseStatus = 200,
          HarchWeb.responseContentType = Text.pack "application/json",
          HarchWeb.responseBody = statusApiBody statusApiData
        }
    SecondRouteDataResult (Right secondRouteData) ->
      HarchWeb.ResponseBody
        { HarchWeb.responseStatus = 200,
          HarchWeb.responseContentType = Text.pack "application/json",
          HarchWeb.responseBody = secondRouteApiBody secondRouteData
        }
    SecondRouteDataResult (Left _) ->
      HarchWeb.ResponseBody
        { HarchWeb.responseStatus = 503,
          HarchWeb.responseContentType = Text.pack "application/json",
          HarchWeb.responseBody = Text.pack "{\"error\":\"second-page-unavailable\"}"
        }
    _ ->
      HarchWeb.ResponseBody
        { HarchWeb.responseStatus = 404,
          HarchWeb.responseContentType = Text.pack "application/json",
          HarchWeb.responseBody = Text.pack "{\"error\":\"not-found\"}"
        }

statusApiBody :: StatusApiData -> Text
statusApiBody statusApiData =
  Text.concat
    [ Text.pack "{\"status\":\"ok\",\"locale\":\"",
      renderLocale (statusApiLocale statusApiData),
      Text.pack "\"}"
    ]

secondRouteApiBody :: SecondRouteData -> Text
secondRouteApiBody secondRouteData =
  Text.concat
    [ Text.pack "{\"summary\":",
      renderJsonString (secondRouteSummary secondRouteData),
      Text.pack ",\"highlights\":",
      renderJsonStringList (secondRouteHighlights secondRouteData),
      Text.pack "}"
    ]

renderJsonStringList :: [Text] -> Text
renderJsonStringList values =
  Text.concat
    [ Text.pack "[",
      Text.intercalate (Text.pack ",") (map renderJsonString values),
      Text.pack "]"
    ]

renderJsonString :: Text -> Text
renderJsonString value =
  Text.concat [Text.pack "\"", value, Text.pack "\""]

renderLocale :: AppLocale -> Text
renderLocale locale =
  case locale of
    English -> Text.pack "en"
    French -> Text.pack "fr"
