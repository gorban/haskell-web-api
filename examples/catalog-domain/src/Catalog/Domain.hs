{-# LANGUAGE OverloadedStrings #-}

-- | A domain-owned module with explicit query and command dependencies.  It
-- knows its local route/action vocabulary only: a composition root chooses
-- its parent route constructor, mount path, security policy, and request
-- context projection.
module Catalog.Domain
  ( CatalogAction (..),
    CatalogActionTarget (..),
    CatalogCommands (..),
    CatalogContext (..),
    CatalogPolicy (..),
    CatalogQueries (..),
    CatalogRoute (..),
    buildCatalogModule,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HarchWeb.Action (ActionCodec, post, singleActionCodecWithMetadata)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (RequireAuthorized),
    EndpointProtocol (ActionEndpoint, HtmlEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Markup (element, headingOneTag, text)
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet),
    RouteParseResult (..),
    RouteRequest (..),
    routeMethodPolicy,
  )
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server
  ( ActionNavigation (StayOnCurrentRoute),
    ClientActionRequest (..),
    ClientActionResponse (..),
    PageResult (RenderedPage),
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Site (RouteDefinition (..), RouteHandler (PageRouteHandler))
import Network.HTTP.Types qualified as Http

data CatalogRoute = CatalogIndex
  deriving (Eq, Show)

data CatalogActionTarget = RefreshCatalogTarget
  deriving (Eq, Show)

data CatalogAction = RefreshCatalog
  deriving (Eq, Show)

data CatalogPolicy
  = MayReadCatalog
  | MayRefreshCatalog
  deriving (Eq, Show)

-- | The intentionally bounded view the domain needs from a root context.
data CatalogContext = CatalogContext
  { catalogLocaleCode :: Text,
    catalogViewerScope :: Maybe Text
  }
  deriving (Eq, Show)

newtype CatalogQueries = CatalogQueries
  { loadCatalogSummary :: CatalogContext -> IO Text
  }

newtype CatalogCommands = CatalogCommands
  { refreshCatalog :: CatalogContext -> IO Text
  }

buildCatalogModule :: CatalogQueries -> CatalogCommands -> ApplicationModule CatalogRoute CatalogActionTarget CatalogAction CatalogContext CatalogPolicy
buildCatalogModule queries commands =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "catalog",
      moduleOwnsRoute = const True,
      moduleRouteMountChain = const (requiredModuleNameOrDie "catalog" NonEmpty.:| []),
      moduleRouteCodec = catalogRouteCodec,
      moduleDeclaredRoutes = [CatalogIndex],
      moduleEndpoints = catalogRouteDefinition queries,
      moduleActionCodec = catalogActionCodec,
      moduleActionRoute = \_ RefreshCatalogTarget -> Just CatalogIndex,
      moduleHandleAction = catalogActionHandler commands,
      moduleGuards = []
    }

catalogRouteCodec :: RouteCodec CatalogRoute CatalogContext
catalogRouteCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [] -> RouteParsed (RouteRequest CatalogIndex requestContext)
          _ -> RouteNotMatched,
      renderRoute = const (RouteLocation [] []),
      notFoundRequest = RouteRequest CatalogIndex,
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

catalogRouteDefinition :: CatalogQueries -> CatalogRoute -> RouteDefinition CatalogRoute CatalogContext CatalogPolicy
catalogRouteDefinition queries CatalogIndex =
  RouteDefinition
    { routeNavigationLabel = Just "Catalog",
      routeMetadata =
        mkEndpointMetadata
          (requiredEndpointNameOrDie "catalog.index")
          (requiredRouteTemplateOrDie "/")
          HtmlEndpoint
          (RequireAuthorized MayReadCatalog),
      routeMethods = [RouteGet],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = PageRouteHandler $ \_ request -> do
        summary <- loadCatalogSummary queries (requestContext request)
        pure
          ( RenderedPage
              Page
                { pageTitle = "Catalog",
                  pageRoute = CatalogIndex,
                  pageContext = requestContext request,
                  pageBody = element headingOneTag [] [text summary],
                  pageBootstrapHooks = []
                }
          )
    }

catalogActionCodec :: ActionCodec CatalogActionTarget CatalogContext CatalogPolicy CatalogAction
catalogActionCodec =
  singleActionCodecWithMetadata
    RefreshCatalogTarget
    (post "/actions/refresh")
    ( mkEndpointMetadata
        (requiredEndpointNameOrDie "catalog.refresh")
        (requiredRouteTemplateOrDie "/actions/refresh")
        ActionEndpoint
        (RequireAuthorized MayRefreshCatalog)
    )
    (pure RefreshCatalog)

catalogActionHandler :: CatalogCommands -> ClientActionRequest CatalogAction CatalogContext -> IO (Maybe (ClientActionResponse CatalogRoute CatalogContext))
catalogActionHandler commands actionRequest =
  case clientAction actionRequest of
    RefreshCatalog -> do
      _ <- refreshCatalog commands (clientActionContext actionRequest)
      pure
        ( Just
            ClientActionResponse
              { clientActionStatus = Http.status200,
                clientActionPatches = [],
                clientActionFocusId = Nothing,
                clientActionNavigation = StayOnCurrentRoute,
                clientActionHeaders = [],
                clientActionObservabilityAttributes = [],
                clientActionLogEntries = []
              }
        )
