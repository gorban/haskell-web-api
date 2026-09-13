-- | Public, session-free landing page for a browser-detected enhanced-action
-- failure. Its route decoder has already validated both values; the page
-- never looks up a session or account from the display-only reference.
module App.CustomPages.ClientActionFailure
  ( routeDefinition,
  )
where

import App.Routes (TwoPageRoute, twoPageClientActionFailureEndpointMetadata)
import HarchWeb
  ( FailureReference,
    PageResult (RenderedPageWithHeaders),
    PageSecurity,
    RouteMethod (RouteGet),
    RouteRequest (..),
    defaultClientActionFailurePage,
    noStoreNoReferrerPageHeaders,
  )
import HarchWeb qualified
import HarchWeb.Site (RouteDefinition (..), RouteHandler (PageRouteHandler))

routeDefinition :: FailureReference -> RouteDefinition TwoPageRoute () ()
routeDefinition failureReference =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = twoPageClientActionFailureEndpointMetadata,
      routeMethods = [RouteGet],
      routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
      routeHandler = PageRouteHandler (renderFailurePage failureReference)
    }

renderFailurePage :: FailureReference -> PageSecurity -> RouteRequest TwoPageRoute () -> IO (PageResult TwoPageRoute ())
renderFailurePage failureReference _ routeRequest =
  pure
    ( RenderedPageWithHeaders
        noStoreNoReferrerPageHeaders
        (defaultClientActionFailurePage failureReference routeRequest)
    )
