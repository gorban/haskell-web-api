{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.CustomPages.Preview (previewPageDefinition) where

import App.Routes
  ( CustomRoute (..),
    PreviewSlug,
    TwoPageRoute,
    previewSlugText,
    twoPagePreviewEndpointMetadata,
  )
import App.Routes qualified as Routes
import HarchWeb
  ( Page (..),
    RouteRequest (..),
    harch,
  )
import HarchWeb.Site (RouteDefinition)
import HarchWeb.Site qualified as Site

previewPageDefinition :: PreviewSlug -> RouteDefinition TwoPageRoute () ()
previewPageDefinition previewSlug =
  Site.pageRoute twoPagePreviewEndpointMetadata Nothing $ \routeRequest ->
    pure
      Page
        { pageTitle = "Preview: " <> previewSlugText previewSlug,
          pageRoute = Routes.Custom (PreviewPage previewSlug),
          pageContext = requestContext routeRequest,
          pageBody =
            [harch|
              <section data-page="preview">
                <h1>Preview</h1>
                <p>{previewSlugText previewSlug}</p>
              </section>
            |],
          pageBootstrapHooks = []
        }
