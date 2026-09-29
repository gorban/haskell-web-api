{-# LANGUAGE QuasiQuotes #-}

-- | @\/showcase@ — the single-file page-module exemplar.
--
-- Everything this static page owns lives in this one file: its scoped styles,
-- enhancement hooks, and @[harch| … |]@ body. The route is implied by
-- the file name ('WebApi.Pages.Showcase' → @\/showcase@) through
-- 'Core.PageRoutes.Generator'.  This page and 'WebApi.Pages.ShowcaseAlternate'
-- deliberately define the same local class names (@card@, @heading@) with
-- different values, to prove scoped-CSS isolation end to end.
module WebApi.Pages.Showcase (pageDefinition) where

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
    AppRoute (ShowcaseRoute),
    endpointMetadata,
    routeEnhancementHooks,
  )
import WebApi.Route qualified as Route

scope :: CssScope
scope = cssScope "showcase"

pageDefinition :: PageDefinitionContext -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageDefinition context =
  RouteDefinition
    { routeNavigation = const Nothing,
      routeMetadata = endpointMetadata ShowcaseRoute,
      routeMethods = const (routeMethodPolicy [RouteGet]),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler =
        PageRouteHandler $ \_ routeRequest ->
          pure
            ( HarchWeb.RenderedPage $
                Page
                  { pageTitle = appTitlePrefix (pageDefinitionConfig context) <> ": " <> Route.routePageTitle (Route.routeMetadata ShowcaseRoute),
                    pageRoute = HarchWeb.requestRoute routeRequest,
                    pageContext = HarchWeb.requestContext routeRequest,
                    pageBody =
                      [harch|
                          <section data-page="showcase" class={ScopedCssClass scope "root"}>
                            <h1 class={ScopedCssClass scope "heading"}>{text showcaseHeading}</h1>
                            <div class={ScopedCssClass scope "card"}>
                              <p>{text showcaseCard}</p>
                            </div>
                          </section>
                        |] ::
                        Html,
                    pageBootstrapHooks = routeEnhancementHooks (Route.routeMetadata ShowcaseRoute),
                    pageStylesheets = [stylesheet (AssetPath "/assets/styles/pages/showcase.css")]
                  }
            )
    }

showcaseHeading :: Text
showcaseHeading = "Showcase"

showcaseCard :: Text
showcaseCard = "This card is styled by showcase.css only."
