{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | @\/showcase@ — the single-file page-module exemplar.
--
-- Everything this page owns lives in this one file: the typed 'ShowcaseData'
-- model and its loader, the scoped styles declared beside the markup, the
-- enhancement hooks, and the @[harch| … |]@ body.  The route is implied by
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
    AppRoute (ShowcaseRoute),
    endpointMetadata,
    routeEnhancementHooks,
    routeMetadata,
    routePageTitle,
  )

data ShowcaseData = ShowcaseData
  { showcaseHeading :: Text,
    showcaseCard :: Text
  }

loadShowcase :: IO (Either PageFailure ShowcaseData)
loadShowcase =
  pure
    ( Right
        ShowcaseData
          { showcaseHeading = "Showcase",
            showcaseCard = "This card is styled by showcase.css only."
          }
    )

scope :: CssScope
scope = cssScope "showcase"

showcaseModule :: PageModule ShowcaseData
showcaseModule =
  PageModule
    { pageModuleEndpointMetadata = endpointMetadata ShowcaseRoute,
      pageModuleNavLabel = Nothing,
      pageModuleTitle = routePageTitle (routeMetadata ShowcaseRoute),
      pageModuleStylesheets = [stylesheet (AssetPath "/assets/styles/pages/showcase.css")],
      pageModuleHooks = routeEnhancementHooks (routeMetadata ShowcaseRoute),
      pageModuleLoad = loadShowcase,
      pageModuleRender = \loaded ->
        pure
          ( [harch|
              <section data-page="showcase" class={ScopedCssClass scope "root"}>
                <h1 class={ScopedCssClass scope "heading"}>{text (showcaseHeading loaded)}</h1>
                <div class={ScopedCssClass scope "card"}>
                  <p>{text (showcaseCard loaded)}</p>
                </div>
              </section>
            |] ::
              Html
          )
    }

pageDefinition :: AppConfig -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageDefinition = pageModuleDefinition showcaseModule
