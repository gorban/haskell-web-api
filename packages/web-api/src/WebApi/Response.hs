{-# LANGUAGE OverloadedStrings #-}

module WebApi.Response
  ( FailureDiagnostics (..),
    FailureSurface (..),
    jsonErrorBody,
    jsonText,
    pageFailureDiagnostics,
    renderLocale,
    spacesLocation,
    apiNotFoundResponse,
    secondRouteApiBody,
    selectResponseWithDatabaseAndAccountWorkflow,
    selectResponseWithDatabase,
    selectResponse,
    statusApiBody,
    toHarchDatabaseOperation,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Database qualified as HarchDatabase
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (AppConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), PageRepository, defaultPageRepository)
import WebApi.Page (renderPageFromRouteData, renderProfilePageWithState, renderUnavailableProfilePage)
import WebApi.Profile (ProfileLoadError (..), loadProfileForPrincipal)
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
    selectRouteDataSelectionWithDatabase,
  )

-- | Select a page-handler outcome before Harch's single renderer attaches
-- per-document security.  API and redirect routes are dispatched separately
-- by 'WebApi.App'; this function cannot construct a final page response.
selectResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.PageResult AppRoute AppRequestContext)
selectResponse config =
  selectResponseWithDatabase config defaultPageRepository

selectResponseWithDatabase :: AppConfig -> PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.PageResult AppRoute AppRequestContext)
selectResponseWithDatabase config pageRepository routeRequest =
  fmap
    (renderPageResponseFromRouteDataSelection config routeRequest)
    (selectRouteDataSelectionWithDatabase pageRepository routeRequest)

selectResponseWithDatabaseAndAccountWorkflow :: AppConfig -> PageRepository -> AccountWorkflow -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.PageResult AppRoute AppRequestContext)
selectResponseWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow routeRequest =
  if isProfilePageRequest routeRequest
    then selectProfileResponse config accountWorkflow routeRequest
    else selectResponseWithDatabase config pageRepository routeRequest

isProfilePageRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Bool
isProfilePageRequest routeRequest =
  HarchWeb.requestRoute routeRequest == ProfileRoute

selectProfileResponse :: AppConfig -> AccountWorkflow -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.PageResult AppRoute AppRequestContext)
selectProfileResponse config accountWorkflow routeRequest = do
  loadedProfile <-
    loadProfileForPrincipal
      (accountWorkflowProfileStore accountWorkflow)
      (requestAccountPrincipal (HarchWeb.requestContext routeRequest))
  pure $
    case loadedProfile of
      Right profileState -> HarchWeb.RenderedPage (renderProfilePageWithState config routeRequest profileState)
      Left (ProfileAccountStoreError _) ->
        HarchWeb.RenderedPageWithMetadata
          (pageErrorResponseMetadata profileFailureDiagnostics)
          (renderUnavailableProfilePage config routeRequest)

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
  HarchWeb.PageResult AppRoute AppRequestContext
renderPageResponseFromRouteDataSelection config routeRequest routeDataSelection =
  case routeData of
    SecondRouteDataResult (Left databaseError) ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in HarchWeb.RenderedPageWithMetadata
            (pageErrorResponseMetadata (pageFailureDiagnostics PageFailureSurface "/second" "second-page" routeDataDatabaseOperationsValue databaseError))
            renderedPage
    _ ->
      let renderedPage = renderPageFromRouteData config routeRequest routeData
       in if null routeDataDatabaseOperationsValue
            then HarchWeb.RenderedPage renderedPage
            else HarchWeb.RenderedPageWithMetadata (pageSuccessResponseMetadata routeDataDatabaseOperationsValue) renderedPage
  where
    routeData = routeDataResult routeDataSelection
    routeDataDatabaseOperationsValue = routeDataDatabaseOperations routeDataSelection

statusApiBody :: AppLocale -> JsonEncoding.Encoding
statusApiBody locale =
  JsonEncoding.pairs
    ( JsonEncoding.pair "status" (Aeson.toEncoding ("ok" :: Text))
        <> JsonEncoding.pair "locale" (Aeson.toEncoding (renderLocale locale))
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

apiNotFoundResponse :: HarchWeb.ResponseBody
apiNotFoundResponse =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = Http.status404,
      HarchWeb.responseContentType = "application/json",
      HarchWeb.responseBody = jsonText (jsonErrorBody "not-found"),
      HarchWeb.responseObservabilityAttributes = [],
      HarchWeb.responseLogEntries = [],
      HarchWeb.responseDatabaseOperations = []
    }

jsonText :: JsonEncoding.Encoding -> Text
jsonText = TextEncoding.decodeUtf8 . LazyByteString.toStrict . JsonEncoding.encodingToLazyByteString

pageSuccessResponseMetadata :: [DatabaseOperation] -> HarchWeb.ResponseBody
pageSuccessResponseMetadata databaseOperations =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = Http.status200,
      HarchWeb.responseContentType = "text/html; charset=utf-8",
      HarchWeb.responseBody = "",
      HarchWeb.responseObservabilityAttributes = [],
      HarchWeb.responseLogEntries = [],
      HarchWeb.responseDatabaseOperations = map toHarchDatabaseOperation databaseOperations
    }

pageErrorResponseMetadata :: FailureDiagnostics -> HarchWeb.ResponseBody
pageErrorResponseMetadata diagnostics =
  HarchWeb.ResponseBody
    { HarchWeb.responseStatus = Http.status500,
      HarchWeb.responseContentType = "text/html; charset=utf-8",
      HarchWeb.responseBody = "",
      HarchWeb.responseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      HarchWeb.responseLogEntries = diagnosticsLogEntries diagnostics,
      HarchWeb.responseDatabaseOperations = diagnosticsDatabaseOperations diagnostics
    }

data FailureDiagnostics = FailureDiagnostics
  { diagnosticsObservabilityAttributes :: [Observability.ObservabilityAttribute],
    diagnosticsLogEntries :: [Text],
    diagnosticsDatabaseOperations :: [HarchDatabase.DatabaseOperation]
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
              Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.failure.code",
              Observability.attributeValue = Observability.TextAttribute "database.second-page-data"
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.route",
              Observability.attributeValue = Observability.TextAttribute routePath
            },
          Observability.ObservabilityAttribute
            { Observability.attributeName = "app.surface",
              Observability.attributeValue = Observability.TextAttribute (renderFailureSurface failureSurface)
            }
        ],
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
        ],
      diagnosticsDatabaseOperations = map toHarchDatabaseOperation databaseOperations
    }

profileFailureDiagnostics :: FailureDiagnostics
profileFailureDiagnostics =
  FailureDiagnostics
    { diagnosticsObservabilityAttributes =
        [ Observability.ObservabilityAttribute
            { Observability.attributeName = "error.type",
              Observability.attributeValue = Observability.TextAttribute profileLoadErrorType
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
      diagnosticsLogEntries = ["Profile loading failed: " <> profileLoadErrorType],
      diagnosticsDatabaseOperations = []
    }

profileLoadErrorType :: Text
profileLoadErrorType = "AccountStoreError"

toHarchDatabaseOperation :: DatabaseOperation -> HarchDatabase.DatabaseOperation
toHarchDatabaseOperation databaseOperation =
  HarchDatabase.DatabaseOperation
    { HarchDatabase.databaseOperationSystem = "postgresql",
      HarchDatabase.databaseOperationName = databaseOperationName databaseOperation,
      HarchDatabase.databaseQueryTemplate = databaseQueryTemplate databaseOperation,
      HarchDatabase.databaseOperationStartedAtNanoseconds = databaseOperationStartedAtNanoseconds databaseOperation,
      HarchDatabase.databaseOperationEndedAtNanoseconds = databaseOperationEndedAtNanoseconds databaseOperation
    }

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

renderFailureSurface :: FailureSurface -> Text
renderFailureSurface failureSurface =
  case failureSurface of
    PageFailureSurface -> "page"
    ApiFailureSurface -> "api"
