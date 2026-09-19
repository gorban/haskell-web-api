{-# LANGUAGE OverloadedStrings #-}

module WebApi.Response
  ( FailureDiagnostics (..),
    FailureSurface (..),
    jsonErrorBody,
    jsonText,
    pageFailureDiagnostics,
    renderLocale,
    todoLocation,
    apiNotFoundResponse,
    meApiSuccessBody,
    secondRouteApiBody,
    selectResponseWithDatabaseAndAccountWorkflow,
    selectResponseWithDatabase,
    selectResponse,
    statusApiBody,
    tokenApiSuccessBody,
    toHarchDatabaseOperation,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Database qualified as HarchDatabase
import HarchWeb.Email (emailAddressText)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Username (usernameText)
import Network.HTTP.Types qualified as Http
import WebApi.Account (AccountProfile (..))
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

todoLocation :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
todoLocation routeRequest =
  renderRoutePath
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = TodoRoute,
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

-- | The RFC 6749 section 5.1 successful token response. @scope@ is always
-- present (space-joined, empty when the client used its default scopes)
-- rather than conditionally omitted, since this endpoint's one client is a
-- typed adapter and gains nothing from the human-readability the spec's
-- "OPTIONAL if identical to the scope requested" allowance exists for.
tokenApiSuccessBody :: Text -> [Text] -> Word64 -> JsonEncoding.Encoding
tokenApiSuccessBody accessToken scopes lifetimeSeconds =
  JsonEncoding.pairs
    ( JsonEncoding.pair "access_token" (Aeson.toEncoding accessToken)
        <> JsonEncoding.pair "token_type" (Aeson.toEncoding ("Bearer" :: Text))
        <> JsonEncoding.pair "expires_in" (Aeson.toEncoding lifetimeSeconds)
        <> JsonEncoding.pair "scope" (Aeson.toEncoding (Text.unwords scopes))
    )

-- | The requested account-self resource for @\/api\/me@: the authenticated
-- account's own username and email. A missing username is an ordinary
-- account state (registration does not require one), rendered as JSON
-- @null@ rather than an omitted field, so a client cannot mistake "no
-- username" for "field not yet implemented".
meApiSuccessBody :: AccountProfile -> JsonEncoding.Encoding
meApiSuccessBody profile =
  JsonEncoding.pairs
    ( JsonEncoding.pair "username" (maybe JsonEncoding.null_ (Aeson.toEncoding . usernameText) (accountProfileUsername profile))
        <> JsonEncoding.pair "email" (Aeson.toEncoding (emailAddressText (accountProfileEmail profile)))
    )

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
