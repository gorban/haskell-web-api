module WebApi.Response
  ( selectResponse,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Config (AppConfig)
import WebApi.Page (renderPage)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    RequestSurface (..),
  )

selectResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Response AppRoute AppRequestContext
selectResponse config routeRequest =
  case (HarchWeb.requestRoute routeRequest, requestSurface (HarchWeb.requestContext routeRequest)) of
    (StatusApiRoute, ApiSurface) ->
      HarchWeb.BodyResponse
        HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 200,
            HarchWeb.responseContentType = Text.pack "application/json",
            HarchWeb.responseBody = statusApiBody routeRequest
          }
    (NotFoundRoute, ApiSurface) ->
      HarchWeb.BodyResponse
        HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 404,
            HarchWeb.responseContentType = Text.pack "application/json",
            HarchWeb.responseBody = Text.pack "{\"error\":\"not-found\"}"
          }
    _ ->
      HarchWeb.PageResponse (renderPage config routeRequest)

statusApiBody :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
statusApiBody routeRequest =
  Text.concat
    [ Text.pack "{\"status\":\"ok\",\"locale\":\"",
      renderLocale (requestLocale (HarchWeb.requestContext routeRequest)),
      Text.pack "\"}"
    ]

renderLocale :: AppLocale -> Text
renderLocale locale =
  case locale of
    English -> Text.pack "en"
    French -> Text.pack "fr"
