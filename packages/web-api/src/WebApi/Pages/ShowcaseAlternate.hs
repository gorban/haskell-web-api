{-# LANGUAGE QuasiQuotes #-}

-- | @\/showcase-alternate@ — the deliberate scoped-CSS collision twin.
--
-- This static page defines the same local class names as 'WebApi.Pages.Showcase' —
-- @card@ and @heading@ — with deliberately different values.  If scoping ever
-- regressed, one page would style the other; the browser test asserts each
-- page keeps only its own rules.
module WebApi.Pages.ShowcaseAlternate (pageDefinition) where

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
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext,
    AppRoute (ShowcaseAlternateRoute),
    endpointMetadata,
    routeEnhancementHooks,
  )
import WebApi.Route qualified as Route

scope :: CssScope
scope = cssScope "showcase-alternate"

pageDefinition :: PageDefinitionContext -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageDefinition context =
  RouteDefinition
    { routeNavigation = const Nothing,
      routeMetadata = endpointMetadata ShowcaseAlternateRoute,
      routeMethods = const (routeMethodPolicy [RouteGet]),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler =
        PageRouteHandler $ \_ routeRequest ->
          pure
            ( HarchWeb.RenderedPage $
                Page
                  { pageTitle = appTitlePrefix (pageDefinitionConfig context) <> ": " <> Route.routePageTitle (Route.routeMetadata ShowcaseAlternateRoute),
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
                    pageBootstrapHooks = routeEnhancementHooks (Route.routeMetadata ShowcaseAlternateRoute),
                    pageStylesheets = [stylesheet (AssetPath "/assets/styles/pages/showcase-alternate.css")]
                  }
            )
    }

showcaseAlternateHeading :: Text
showcaseAlternateHeading = "Showcase alternate"

showcaseAlternateCard :: Text
showcaseAlternateCard = "This card is styled by showcase-alternate.css only."
