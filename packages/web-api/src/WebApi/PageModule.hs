{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The single-file page-module authoring unit for the 'WebApi.Pages' family.
--
-- Decision record (2026-09-25, authoring-quality exemplar): a page module is
-- the Haskell analogue of a Svelte single-file component — one @.hs@ file
-- supplies the page title, its scoped styles, its enhancement hooks, its data
-- loading with a typed failure rail, and its @[harch| … |]@ body, and the
-- file name implies the route through 'Core.PageRoutes.Generator'.  The unit
-- is deliberately a thin composition of existing framework pieces
-- ('HarchWeb.Document.Page', 'HarchWeb.Site.pageRoute', 'Stylesheet') rather
-- than a new abstraction: @pageDefinition@ takes the application's 'AppConfig'
-- because page handlers are functions of configuration (the generator's
-- context parameter exists for exactly this shape), while the module's
-- presentation values stay static so 'WebApi.Route.routeMetadata' can project
-- them without a config.  The corresponding stylesheet is colocated by name
-- under @public\/styles\/pages\/@ and is scoped CSS in the 'HarchWeb.CssScope'
-- convention.
module WebApi.PageModule
  ( PageFailure (..),
    PageModule (..),
    pageModuleDefinition,
    pageModulePage,
    renderPageFailure,
  )
where

import Data.Text (Text)
import HarchWeb
  ( EndpointMetadata,
    Html,
    Page (..),
    RouteRequest (..),
    Stylesheet,
    harch,
    text,
  )
import HarchWeb.Site (RouteDefinition)
import HarchWeb.Site qualified as Site
import WebApi.Config (AppConfig, appTitlePrefix)
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext,
    AppRoute,
  )

-- | How a page load can fail.  Expected outcomes are ordinary values, never
-- exceptions; the application renders one shared failure page.
newtype PageFailure
  = PageFailureMessage Text
  deriving (Eq, Show)

-- | Everything one page owns.  The route itself is implied by the module's
-- file name, so the record only carries what the page supplies.
data PageModule loaded = PageModule
  { pageModuleEndpointMetadata :: EndpointMetadata AppAuthorization,
    pageModuleNavLabel :: Maybe Text,
    pageModuleTitle :: Text,
    pageModuleStylesheets :: [Stylesheet],
    pageModuleHooks :: [Text],
    pageModuleLoad :: IO (Either PageFailure loaded),
    pageModuleRender :: loaded -> IO Html
  }

-- | Build the page's 'RouteDefinition' the application mounts.  The load
-- failure rail renders the shared failure page instead of propagating a
-- 'Left' through the handler.
pageModuleDefinition ::
  PageModule loaded -> AppConfig -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageModuleDefinition pageModule config =
  Site.pageRoute
    (pageModuleEndpointMetadata pageModule)
    (pageModuleNavLabel pageModule)
    (\_security request -> pageModulePage pageModule config request)

-- | The page-module handler: run the load with its typed failure rail, then
-- compose the 'Page'. Split from 'pageModuleDefinition' so tests can drive
-- the rail directly.
pageModulePage ::
  PageModule loaded ->
  AppConfig ->
  HarchWeb.RouteRequest AppRoute AppRequestContext ->
  IO (Page AppRoute AppRequestContext)
pageModulePage pageModule config request = do
  let finishPage body =
        Page
          { pageTitle = appTitlePrefix config <> ": " <> pageModuleTitle pageModule,
            pageRoute = requestRoute request,
            pageContext = requestContext request,
            pageBody = body,
            pageBootstrapHooks = pageModuleHooks pageModule,
            pageStylesheets = pageModuleStylesheets pageModule
          }
  pageModuleLoad pageModule >>= \case
    Left failure -> pure (finishPage (renderPageFailure failure))
    Right loaded -> finishPage <$> pageModuleRender pageModule loaded

-- | The one shared load-failure page.
renderPageFailure :: PageFailure -> Html
renderPageFailure failure =
  [harch|
    <section data-page="load-failure">
      <h1>Something went wrong</h1>
      <p>{text (pageFailureText failure)}</p>
    </section>
  |]
  where
    pageFailureText :: PageFailure -> Text
    pageFailureText (PageFailureMessage message) = message
