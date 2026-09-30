{-# LANGUAGE QuasiQuotes #-}

-- | @\/showcase-alternate@ — the deliberate scoped-CSS collision twin.
--
-- This static page defines the same local class names as 'WebApi.Pages.Showcase' —
-- @card@ and @heading@ — with deliberately different values. If scoping ever
-- regressed, one page would style the other; the browser test asserts each
-- page keeps only its own rules. Its generated dispatcher collects the
-- declaration below so both Site and standalone rendering share the page's
-- presentation facts.
module WebApi.Pages.ShowcaseAlternate (pageDefinition, pagePresentation) where

import Data.Text (Text)
import HarchWeb
  ( AssetPath (..),
    CssClass (..),
    CssScope,
    Html,
    Page (..),
    RouteMethod (RouteGet),
    cssScope,
    harch,
    routeMethodPolicy,
    stylesheet,
    text,
    unboundedRouteExecutionPolicy,
  )
import HarchWeb qualified
import HarchWeb.Site (RouteDefinition (..), RouteHandler (PageRouteHandler))
import WebApi.Config (appTitlePrefix)
import WebApi.PageModule (PageDefinitionContext (..))
import WebApi.Route.Endpoint (endpointMetadata)
import WebApi.Route.Types
  ( AppAuthorization,
    AppRequestContext,
    AppRoute (ShowcaseAlternateRoute),
    PagePresentation (..),
  )

scope :: CssScope
scope = cssScope "showcase-alternate"

pagePresentation :: PagePresentation
pagePresentation =
  PagePresentation
    { pagePresentationTitle = const showcaseAlternateHeading,
      pagePresentationNavigation = const Nothing,
      pagePresentationEnhancementHooks = ["web-api-showcase-alternate"]
    }

pageDefinition :: PageDefinitionContext -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageDefinition context =
  RouteDefinition
    { routeNavigation = pagePresentationNavigation pagePresentation,
      routeMetadata = endpointMetadata ShowcaseAlternateRoute,
      routeMethods = const (routeMethodPolicy [RouteGet]),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler =
        PageRouteHandler $ \_ routeRequest ->
          pure
            ( HarchWeb.RenderedPage $
                Page
                  { pageTitle = appTitlePrefix (pageDefinitionConfig context) <> ": " <> pagePresentationTitle pagePresentation (HarchWeb.requestContext routeRequest),
                    pageRoute = HarchWeb.requestRoute routeRequest,
                    pageContext = HarchWeb.requestContext routeRequest,
                    pageBody =
                      [harch|
                          <section data-page="showcase-alternate" class={ScopedCssClass scope "root"}>
                            <h1 class={ScopedCssClass scope "heading"}>{text showcaseAlternateHeading}</h1>
                            <div class={ScopedCssClass scope "card"}>
                              <p>{text showcaseAlternateCard}</p>
                            </div>
                          </section>
                        |] ::
                        Html,
                    pageBootstrapHooks = pagePresentationEnhancementHooks pagePresentation,
                    pageStylesheets = [stylesheet (AssetPath "/assets/styles/pages/showcase-alternate.css")]
                  }
            )
    }

showcaseAlternateHeading :: Text
showcaseAlternateHeading = "Showcase alternate"

showcaseAlternateCard :: Text
showcaseAlternateCard = "This card is styled by showcase-alternate.css only."
