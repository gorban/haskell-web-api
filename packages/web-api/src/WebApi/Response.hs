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
import WebApi.Database (DatabaseEffect, DatabaseError (..), DatabaseOperation (..), defaultDatabaseEffect)
import WebApi.Page (renderPageFromRouteData)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    RequestSurface (..),
    renderRoutePath,
  )
import WebApi.RouteData
  ( RouteDataResult (..),
    RouteDataSelection (..),
    SecondRouteData (..),
    StatusApiData (..),
    selectRouteDataSelectionWithDatabase,
  )

selectResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectResponse config =
  selectResponseWithDatabase config defaultDatabaseEffect

selectResponseWithDatabase :: AppConfig -> DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectResponseWithDatabase config databaseEffect routeRequest =
  if isHomePageRequest routeRequest
    then pure (HarchWeb.redirectResponse 302 (spacesLocation routeRequest))
    else
      fmap
        ( \routeDataSelection ->
            case requestSurface (HarchWeb.requestContext routeRequest) of
              ApiSurface ->
                HarchWeb.BodyResponse (renderApiResponseFromRouteDataSelection routeDataSelection)
              PageSurface ->
                renderPageResponseFromRouteDataSelection config routeRequest routeDataSelection
        )
        (selectRouteDataSelectionWithDatabase databaseEffect routeRequest)

isHomePageRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Bool
isHomePageRequest routeRequest =
  HarchWeb.requestRoute routeRequest == HomeRoute
    && requestSurface (HarchWeb.requestContext routeRequest) == PageSurface

spacesLocation :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
spacesLocation routeRequest =
  renderRoutePath
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = SpacesRoute,
        HarchWeb.requestContext = HarchWeb.requestContext routeRequest
      }

renderPageResponseFromRouteDataSelection ::
  AppConfig ->
  HarchWeb.RouteRequest AppRoute AppRequestContext ->
  RouteDataSelection ->
  HarchWeb.Response AppRoute AppRequestContext
renderPageResponseFromRouteDataSelection config routeRequest routeDataSelection =
  case routeData of
    HomeRouteDataResult (Left databaseError) ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in HarchWeb.PageResponseWithMetadata
            (pageErrorResponseMetadata (pageFailureDiagnostics PageSurface "/" "home-page" routeDataDatabaseOperationsValue databaseError))
            renderedPage
    SecondRouteDataResult (Left databaseError) ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in HarchWeb.PageResponseWithMetadata
            (pageErrorResponseMetadata (pageFailureDiagnostics PageSurface "/second" "second-page" routeDataDatabaseOperationsValue databaseError))
            renderedPage
    _ ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in if null routeDataDatabaseOperationsValue
            then HarchWeb.PageResponse renderedPage
            else HarchWeb.PageResponseWithMetadata (pageSuccessResponseMetadata routeDataDatabaseOperationsValue) renderedPage
  where
    routeData = routeDataResult routeDataSelection
    routeDataDatabaseOperationsValue = routeDataDatabaseOperations routeDataSelection

renderApiResponseFromRouteData :: RouteDataResult -> HarchWeb.ResponseBody
renderApiResponseFromRouteData =
  renderApiResponseFromRouteDataWithOperations []

renderApiResponseFromRouteDataSelection :: RouteDataSelection -> HarchWeb.ResponseBody
renderApiResponseFromRouteDataSelection routeDataSelection =
  renderApiResponseFromRouteDataWithOperations
    (routeDataDatabaseOperations routeDataSelection)
    (routeDataResult routeDataSelection)

renderApiResponseFromRouteDataWithOperations :: [DatabaseOperation] -> RouteDataResult -> HarchWeb.ResponseBody
renderApiResponseFromRouteDataWithOperations databaseOperations routeData =
  case routeData of
    StatusApiDataResult statusApiData ->
      jsonResponseBodyWithOperations 200 (statusApiBody statusApiData) databaseOperations
    SecondRouteDataResult (Right secondRouteData) ->
      jsonResponseBodyWithOperations 200 (secondRouteApiBody secondRouteData) databaseOperations
    SecondRouteDataResult (Left databaseError) ->
      jsonErrorResponseBody 503 "{\"error\":\"second-page-unavailable\"}" (pageFailureDiagnostics ApiSurface "/second" "second-page" databaseOperations databaseError)
    _ ->
      jsonResponseBodyWithOperations 404 "{\"error\":\"not-found\"}" databaseOperations

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
    Spanish -> "es"

jsonResponseBodyWithOperations :: Int -> Text -> [DatabaseOperation] -> HarchWeb.ResponseBody
jsonResponseBodyWithOperations statusCode bodyText databaseOperations =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = statusCode,
      HarchWeb.responseContentType = "application/json",
      HarchWeb.responseBody = bodyText,
      HarchWeb.responseObservabilityAttributes = databaseOperationObservabilityAttributes databaseOperations,
      HarchWeb.responseLogEntries = []
    }

jsonErrorResponseBody :: Int -> Text -> FailureDiagnostics -> HarchWeb.ResponseBody
jsonErrorResponseBody statusCode bodyText diagnostics =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = statusCode,
      HarchWeb.responseContentType = "application/json",
      HarchWeb.responseBody = bodyText,
      HarchWeb.responseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      HarchWeb.responseLogEntries = diagnosticsLogEntries diagnostics
    }

pageSuccessResponseMetadata :: [DatabaseOperation] -> HarchWeb.ResponseBody
pageSuccessResponseMetadata databaseOperations =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = 200,
      HarchWeb.responseContentType = "text/html; charset=utf-8",
      HarchWeb.responseBody = "",
      HarchWeb.responseObservabilityAttributes = databaseOperationObservabilityAttributes databaseOperations,
      HarchWeb.responseLogEntries = []
    }

pageErrorResponseMetadata :: FailureDiagnostics -> HarchWeb.ResponseBody
pageErrorResponseMetadata diagnostics =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = 500,
      HarchWeb.responseContentType = "text/html; charset=utf-8",
      HarchWeb.responseBody = "",
      HarchWeb.responseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      HarchWeb.responseLogEntries = diagnosticsLogEntries diagnostics
    }

data FailureDiagnostics = FailureDiagnostics
  { diagnosticsObservabilityAttributes :: [Observability.ObservabilityAttribute],
    diagnosticsLogEntries :: [Text]
  }

pageFailureDiagnostics :: RequestSurface -> Text -> Text -> [DatabaseOperation] -> DatabaseError -> FailureDiagnostics
pageFailureDiagnostics requestSurfaceValue routePath routeLabel databaseOperations databaseError =
  FailureDiagnostics
    { diagnosticsObservabilityAttributes =
        [ Observability.ObservabilityAttribute
            { Observability.attributeName = "error.type",
              Observability.attributeValue = Observability.TextAttribute (databaseErrorType databaseError)
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.failure.code",
              Observability.attributeValue = Observability.TextAttribute (databaseFailureCode databaseError)
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.route",
              Observability.attributeValue = Observability.TextAttribute routePath
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.surface",
              Observability.attributeValue = Observability.TextAttribute (renderRequestSurface requestSurfaceValue)
            }
        ]
          <> databaseOperationObservabilityAttributes databaseOperations,
      diagnosticsLogEntries =
        [ Text.concat
            [ "Database failure while rendering required ",
              routeLabel,
              " ",
              renderRequestSurface requestSurfaceValue,
              " response",
              renderDatabaseOperationsSuffix databaseOperations,
              ": ",
              Text.pack (show databaseError)
            ]
        ]
    }

databaseOperationObservabilityAttributes :: [DatabaseOperation] -> [Observability.ObservabilityAttribute]
databaseOperationObservabilityAttributes =
  concatMap databaseOperationObservabilityEntries

databaseOperationObservabilityEntries :: DatabaseOperation -> [Observability.ObservabilityAttribute]
databaseOperationObservabilityEntries databaseOperation =
  [ Observability.ObservabilityAttribute
      { Observability.attributeName = "db.system",
        Observability.attributeValue = Observability.TextAttribute "postgresql"
      },
    Observability.ObservabilityAttribute
      { Observability.attributeName = "db.operation.name",
        Observability.attributeValue = Observability.TextAttribute (databaseOperationName databaseOperation)
      },
    Observability.ObservabilityAttribute
      { Observability.attributeName = "db.query.template",
        Observability.attributeValue = Observability.TextAttribute (databaseQueryTemplate databaseOperation)
      }
  ]
    <> maybeDatabaseOperationTimingAttributes databaseOperation

maybeDatabaseOperationTimingAttributes :: DatabaseOperation -> [Observability.ObservabilityAttribute]
maybeDatabaseOperationTimingAttributes databaseOperation =
  case (databaseOperationStartedAtNanoseconds databaseOperation, databaseOperationEndedAtNanoseconds databaseOperation) of
    (Just startedAt, Just endedAt) ->
      [ Observability.ObservabilityAttribute
          { Observability.attributeName = "db.operation.start_monotonic_ns",
            Observability.attributeValue = Observability.IntAttribute (fromIntegral startedAt)
          },
        Observability.ObservabilityAttribute
          { Observability.attributeName = "db.operation.duration_ns",
            Observability.attributeValue = Observability.IntAttribute (fromIntegral (endedAt - min startedAt endedAt))
          }
      ]
    _ -> []

renderDatabaseOperationsSuffix :: [DatabaseOperation] -> Text
renderDatabaseOperationsSuffix databaseOperations =
  case databaseOperations of
    [] -> ""
    _ ->
      " after database operations ["
        <> Text.intercalate
          ", "
          [ databaseOperationName databaseOperation
              <> " ("
              <> databaseQueryTemplate databaseOperation
              <> ")"
          | databaseOperation <- databaseOperations
          ]
        <> "]"

databaseErrorType :: DatabaseError -> Text
databaseErrorType databaseError =
  case databaseError of
    HomePageDataError _ -> "HomePageDataError"
    SecondPageDataError _ -> "SecondPageDataError"

databaseFailureCode :: DatabaseError -> Text
databaseFailureCode databaseError =
  case databaseError of
    HomePageDataError _ -> "database.home-page-data"
    SecondPageDataError _ -> "database.second-page-data"

renderRequestSurface :: RequestSurface -> Text
renderRequestSurface requestSurfaceValue =
  case requestSurfaceValue of
    PageSurface -> "page"
    ApiSurface -> "api"
