{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | @\/showcase-alternate@ — the deliberate scoped-CSS collision twin.
--
-- This page defines the same local class names as 'WebApi.Pages.Showcase' —
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
    cssScope,
    harch,
    stylesheet,
    text,
  )
import HarchWeb.Site (RouteDefinition)
import WebApi.Config (AppConfig)
import WebApi.PageModule
  ( PageFailure,
    PageModule (..),
    pageModuleDefinition,
  )
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext,
    AppRoute (ShowcaseAlternateRoute),
    endpointMetadata,
    routeEnhancementHooks,
    routeMetadata,
    routePageTitle,
  )

data ShowcaseAlternateData = ShowcaseAlternateData
  { showcaseAlternateHeading :: Text,
    showcaseAlternateCard :: Text
  }

loadShowcaseAlternate :: IO (Either PageFailure ShowcaseAlternateData)
loadShowcaseAlternate =
  pure
    ( Right
        ShowcaseAlternateData
          { showcaseAlternateHeading = "Showcase alternate",
            showcaseAlternateCard = "This card is styled by showcase-alternate.css only."
          }
    )

scope :: CssScope
scope = cssScope "showcase-alternate"

showcaseAlternateModule :: PageModule ShowcaseAlternateData
showcaseAlternateModule =
  PageModule
    { pageModuleEndpointMetadata = endpointMetadata ShowcaseAlternateRoute,
      pageModuleNavLabel = Nothing,
      pageModuleTitle = routePageTitle (routeMetadata ShowcaseAlternateRoute),
      pageModuleStylesheets = [stylesheet (AssetPath "/assets/styles/pages/showcase-alternate.css")],
      pageModuleHooks = routeEnhancementHooks (routeMetadata ShowcaseAlternateRoute),
      pageModuleLoad = loadShowcaseAlternate,
      pageModuleRender = \loaded ->
        pure
          ( [harch|
              <section data-page="showcase-alternate" class={ScopedCssClass scope "root"}>
                <h1 class={ScopedCssClass scope "heading"}>{text (showcaseAlternateHeading loaded)}</h1>
                <div class={ScopedCssClass scope "card"}>
                  <p>{text (showcaseAlternateCard loaded)}</p>
                </div>
              </section>
            |] ::
              Html
          )
    }

pageDefinition :: AppConfig -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageDefinition = pageModuleDefinition showcaseAlternateModule
