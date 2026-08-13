{-# LANGUAGE OverloadedStrings #-}

module WebApi.Response
  ( FailureDiagnostics (..),
    FailureSurface (..),
    databaseOperationObservabilityAttributes,
    jsonErrorBody,
    jsonText,
    pageFailureDiagnostics,
    renderApiResponseFromRouteData,
    renderLocale,
    secondRouteApiBody,
    selectResponseWithDatabaseAndAccountWorkflow,
    selectResponseWithDatabase,
    selectResponse,
    statusApiBody,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (AppConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), PageRepository, defaultPageRepository)
import WebApi.Page (renderPageFromRouteData, renderProfilePageWithState, renderUnavailableProfilePage)
import WebApi.Profile (ProfileLoadError (..), loadProfile)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
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
  selectResponseWithDatabase config defaultPageRepository

selectResponseWithDatabase :: AppConfig -> PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectResponseWithDatabase config pageRepository routeRequest =
  if isHomePageRequest routeRequest
    then pure (HarchWeb.redirectResponse 302 (spacesLocation routeRequest))
    else
      fmap
        ( \routeDataSelection ->
            case HarchWeb.requestRoute routeRequest of
              Api _ ->
                HarchWeb.BodyResponse (renderApiResponseFromRouteDataSelection routeDataSelection)
              Page _ ->
                renderPageResponseFromRouteDataSelection config routeRequest routeDataSelection
        )
        (selectRouteDataSelectionWithDatabase pageRepository routeRequest)

selectResponseWithDatabaseAndAccountWorkflow :: AppConfig -> PageRepository -> AccountWorkflow -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectResponseWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow routeRequest =
  if isProfilePageRequest routeRequest
    then selectProfileResponse config accountWorkflow routeRequest
    else selectResponseWithDatabase config pageRepository routeRequest

isProfilePageRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Bool
isProfilePageRequest routeRequest =
  HarchWeb.requestRoute routeRequest == ProfileRoute

selectProfileResponse :: AppConfig -> AccountWorkflow -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Response AppRoute AppRequestContext)
selectProfileResponse config accountWorkflow routeRequest = do
  nowNanoseconds <- accountWorkflowClock accountWorkflow
  loadedProfile <-
    loadProfile
      (accountWorkflowSessionStore accountWorkflow)
      (accountWorkflowProfileStore accountWorkflow)
      nowNanoseconds
      (requestSessionId (HarchWeb.requestContext routeRequest))
  pure $
    case loadedProfile of
      Right profileState -> HarchWeb.PageResponse (renderProfilePageWithState config routeRequest profileState)
      Left profileLoadError ->
        HarchWeb.PageResponseWithMetadata
          (pageErrorResponseMetadata (profileFailureDiagnostics profileLoadError))
          (renderUnavailableProfilePage config routeRequest)

isHomePageRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Bool
isHomePageRequest routeRequest =
  HarchWeb.requestRoute routeRequest == HomeRoute

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
    SecondRouteDataResult (Left databaseError) ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in HarchWeb.PageResponseWithMetadata
            (pageErrorResponseMetadata (pageFailureDiagnostics PageFailureSurface "/second" "second-page" routeDataDatabaseOperationsValue databaseError))
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
      jsonErrorResponseBody
        503
        (jsonErrorBody "second-page-unavailable")
        (pageFailureDiagnostics ApiFailureSurface "/second" "second-page" databaseOperations databaseError)
    _ ->
      jsonResponseBodyWithOperations 404 (jsonErrorBody "not-found") databaseOperations

statusApiBody :: StatusApiData -> JsonEncoding.Encoding
statusApiBody statusApiData =
  JsonEncoding.pairs
    ( JsonEncoding.pair "status" (Aeson.toEncoding ("ok" :: Text))
        <> JsonEncoding.pair "locale" (Aeson.toEncoding (renderLocale (statusApiLocale statusApiData)))
    )

secondRouteApiBody :: SecondRouteData -> JsonEncoding.Encoding
secondRouteApiBody secondRouteData =
  JsonEncoding.pairs
    ( JsonEncoding.pair "summary" (Aeson.toEncoding (secondRouteSummary secondRouteData))
        <> JsonEncoding.pair "highlights" (Aeson.toEncoding (secondRouteHighlights secondRouteData))
    )

jsonErrorBody :: Text -> JsonEncoding.Encoding
jsonErrorBody errorCode =
  JsonEncoding.pairs (JsonEncoding.pair "error" (Aeson.toEncoding errorCode))

renderLocale :: AppLocale -> Text
renderLocale locale =
  case locale of
    English -> "en"
    Spanish -> "es"

jsonResponseBodyWithOperations :: Int -> JsonEncoding.Encoding -> [DatabaseOperation] -> HarchWeb.ResponseBody
jsonResponseBodyWithOperations statusCode bodyValue databaseOperations =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = statusCode,
      HarchWeb.responseContentType = "application/json",
      HarchWeb.responseBody = jsonText bodyValue,
      HarchWeb.responseObservabilityAttributes = databaseOperationObservabilityAttributes databaseOperations,
      HarchWeb.responseLogEntries = []
    }

jsonErrorResponseBody :: Int -> JsonEncoding.Encoding -> FailureDiagnostics -> HarchWeb.ResponseBody
jsonErrorResponseBody statusCode bodyValue diagnostics =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = statusCode,
      HarchWeb.responseContentType = "application/json",
      HarchWeb.responseBody = jsonText bodyValue,
      HarchWeb.responseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      HarchWeb.responseLogEntries = diagnosticsLogEntries diagnostics
    }

jsonText :: JsonEncoding.Encoding -> Text
jsonText = TextEncoding.decodeUtf8 . LazyByteString.toStrict . JsonEncoding.encodingToLazyByteString

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

data FailureSurface
  = PageFailureSurface
  | ApiFailureSurface

pageFailureDiagnostics :: FailureSurface -> Text -> Text -> [DatabaseOperation] -> DatabaseError -> FailureDiagnostics
pageFailureDiagnostics failureSurface routePath routeLabel databaseOperations databaseError =
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
              Observability.attributeValue = Observability.TextAttribute (renderFailureSurface failureSurface)
            }
        ]
          <> databaseOperationObservabilityAttributes databaseOperations,
      diagnosticsLogEntries =
        [ Text.concat
            [ "Database failure while rendering required ",
              routeLabel,
              " ",
              renderFailureSurface failureSurface,
              " response",
              renderDatabaseOperationsSuffix databaseOperations,
              ": ",
              Text.pack (show databaseError)
            ]
        ]
    }

profileFailureDiagnostics :: ProfileLoadError -> FailureDiagnostics
profileFailureDiagnostics profileLoadError =
  FailureDiagnostics
    { diagnosticsObservabilityAttributes =
        [ Observability.ObservabilityAttribute
            { Observability.attributeName = "error.type",
              Observability.attributeValue = Observability.TextAttribute (profileLoadErrorType profileLoadError)
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.failure.code",
              Observability.attributeValue = Observability.TextAttribute "profile.load"
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.route",
              Observability.attributeValue = Observability.TextAttribute "/profile"
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.surface",
              Observability.attributeValue = Observability.TextAttribute "page"
            }
        ],
      diagnosticsLogEntries = ["Profile loading failed: " <> profileLoadErrorType profileLoadError]
    }

profileLoadErrorType :: ProfileLoadError -> Text
profileLoadErrorType profileLoadError =
  case profileLoadError of
    ProfileSessionStoreError _ -> "AccountSessionStoreError"
    ProfileAccountStoreError _ -> "AccountStoreError"

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

renderFailureSurface :: FailureSurface -> Text
renderFailureSurface failureSurface =
  case failureSurface of
    PageFailureSurface -> "page"
    ApiFailureSurface -> "api"
