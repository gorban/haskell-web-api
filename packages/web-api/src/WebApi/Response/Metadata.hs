-- | Page and API response metadata for database-backed outcomes. This module
-- depends on the application domain and Harch's response types, not the route
-- codec facade, so discovered pages can attach diagnostics without creating a
-- dependency cycle through the generated page dispatcher.
module WebApi.Response.Metadata
  ( FailureDiagnostics (..),
    FailureSurface (..),
    pageFailureDiagnostics,
    pageErrorResponseMetadata,
    pageSuccessResponseMetadata,
    toHarchDatabaseOperation,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Database qualified as HarchDatabase
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import WebApi.Database (DatabaseError, DatabaseOperation (..))

data FailureDiagnostics = FailureDiagnostics
  { diagnosticsObservabilityAttributes :: [Observability.ObservabilityAttribute],
    diagnosticsLogEntries :: [Text],
    diagnosticsDatabaseOperations :: [HarchDatabase.DatabaseOperation]
  }

data FailureSurface
  = PageFailureSurface
  | ApiFailureSurface

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
