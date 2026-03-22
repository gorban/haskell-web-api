{-# LANGUAGE OverloadedStrings #-}

module WebApi.Response
  ( renderApiResponseFromRouteData,
    selectResponseWithDatabase,
    selectResponse,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import WebApi.Config (AppConfig)
import WebApi.Database (DatabaseEffect, DatabaseError (..), defaultDatabaseEffect)
import WebApi.Page (renderPageFromRouteData)
import WebApi.PageShell (buildAppPageShell)
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
            renderPageResponseFromRouteData config routeRequest routeData
    )
    (selectRouteDataWithDatabase databaseEffect routeRequest)

renderPageResponseFromRouteData ::
  AppConfig ->
  HarchWeb.RouteRequest AppRoute AppRequestContext ->
  RouteDataResult ->
  HarchWeb.Response AppRoute AppRequestContext
renderPageResponseFromRouteData config routeRequest routeData =
  case routeData of
    SecondRouteDataResult (Left databaseError) ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in HarchWeb.BodyResponse
            (htmlErrorResponseBody (buildAppPageShell config renderedPage) (secondPageFailureDiagnostics PageSurface databaseError))
    _ ->
      HarchWeb.PageResponse (renderPageFromRouteData config routeRequest routeData)

renderApiResponseFromRouteData :: RouteDataResult -> HarchWeb.ResponseBody
renderApiResponseFromRouteData routeData =
  case routeData of
    StatusApiDataResult statusApiData ->
      jsonResponseBody 200 (statusApiBody statusApiData)
    SecondRouteDataResult (Right secondRouteData) ->
      jsonResponseBody 200 (secondRouteApiBody secondRouteData)
    SecondRouteDataResult (Left databaseError) ->
      jsonErrorResponseBody 503 "{\"error\":\"second-page-unavailable\"}" (secondPageFailureDiagnostics ApiSurface databaseError)
    _ ->
      jsonResponseBody 404 "{\"error\":\"not-found\"}"

statusApiBody :: StatusApiData -> Text
statusApiBody statusApiData =
  Text.concat
    [ "{\"status\":\"ok\",\"locale\":\"",
      renderLocale (statusApiLocale statusApiData),
      "\"}"
    ]

secondRouteApiBody :: SecondRouteData -> Text
secondRouteApiBody secondRouteData =
  Text.concat
    [ "{\"summary\":",
      renderJsonString (secondRouteSummary secondRouteData),
      ",\"highlights\":",
      renderJsonStringList (secondRouteHighlights secondRouteData),
      "}"
    ]

renderJsonStringList :: [Text] -> Text
renderJsonStringList values =
  Text.concat
    [ "[",
      Text.intercalate "," (map renderJsonString values),
      "]"
    ]

renderJsonString :: Text -> Text
renderJsonString value =
  Text.concat ["\"", value, "\""]

renderLocale :: AppLocale -> Text
renderLocale locale =
  case locale of
    English -> "en"
    French -> "fr"

jsonResponseBody :: Int -> Text -> HarchWeb.ResponseBody
jsonResponseBody statusCode bodyText =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = statusCode,
      HarchWeb.responseContentType = "application/json",
      HarchWeb.responseBody = bodyText,
      HarchWeb.responseObservabilityAttributes = [],
      HarchWeb.responseLogEntries = []
    }

jsonErrorResponseBody :: Int -> Text -> FailureDiagnostics -> HarchWeb.ResponseBody
jsonErrorResponseBody statusCode bodyText diagnostics =
  (jsonResponseBody statusCode bodyText)
    { HarchWeb.responseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      HarchWeb.responseLogEntries = diagnosticsLogEntries diagnostics
    }

htmlErrorResponseBody :: Text -> FailureDiagnostics -> HarchWeb.ResponseBody
htmlErrorResponseBody bodyText diagnostics =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = 500,
      HarchWeb.responseContentType = "text/html; charset=utf-8",
      HarchWeb.responseBody = bodyText,
      HarchWeb.responseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      HarchWeb.responseLogEntries = diagnosticsLogEntries diagnostics
    }

data FailureDiagnostics = FailureDiagnostics
  { diagnosticsObservabilityAttributes :: [Observability.ObservabilityAttribute],
    diagnosticsLogEntries :: [Text]
  }

secondPageFailureDiagnostics :: RequestSurface -> DatabaseError -> FailureDiagnostics
secondPageFailureDiagnostics requestSurfaceValue databaseError =
  FailureDiagnostics
    { diagnosticsObservabilityAttributes =
        [ Observability.ObservabilityAttribute
            { Observability.attributeName = "exception.type",
              Observability.attributeValue = Observability.TextAttribute (databaseErrorType databaseError)
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "exception.message",
              Observability.attributeValue = Observability.TextAttribute (databaseErrorMessage databaseError)
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.route",
              Observability.attributeValue = Observability.TextAttribute "/second"
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.surface",
              Observability.attributeValue = Observability.TextAttribute (renderRequestSurface requestSurfaceValue)
            }
        ],
      diagnosticsLogEntries =
        [ Text.concat
            [ "Database failure while rendering required second-page ",
              renderRequestSurface requestSurfaceValue,
              " response: ",
              Text.pack (show databaseError)
            ]
        ]
    }

databaseErrorType :: DatabaseError -> Text
databaseErrorType databaseError =
  case databaseError of
    HomePageDataError _ -> "HomePageDataError"
    SecondPageDataError _ -> "SecondPageDataError"

databaseErrorMessage :: DatabaseError -> Text
databaseErrorMessage databaseError =
  case databaseError of
    HomePageDataError message -> message
    SecondPageDataError message -> message

renderRequestSurface :: RequestSurface -> Text
renderRequestSurface requestSurfaceValue =
  case requestSurfaceValue of
    PageSurface -> "page"
    ApiSurface -> "api"
