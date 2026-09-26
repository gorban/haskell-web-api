{-# LANGUAGE OverloadedStrings #-}

-- | The @/docs@ Swagger UI page as an ordinary typed application surface
-- (AHI-4E slice 6). The page renders complete SSR with a script-free
-- fallback and an enhancement mount; the pinned self-hosted renderer,
-- stylesheet, and behavior module are the harch-web-openapi package's
-- assets, mounted under @/docs/assets@. Every URL is applied through the
-- application's path prefix at render time, so a deployed prefix cannot
-- leave the page pointing at root-absolute asset locations.
module WebApi.DocsSwagger
  ( docsSwaggerPage,
    docsSwaggerUiProps,
  )
where

import HarchWeb (Page, RouteRequest (..))
import HarchWeb qualified
import HarchWeb.OpenApi.Swagger
  ( SwaggerUiProps (..),
    defaultSwaggerUiProps,
    requiredSwaggerSpecUrlOrDie,
    swaggerUiFallback,
    swaggerUiPage,
    swaggerUiStylesheet,
  )
import WebApi.Route (AppRequestContext, AppRoute, requestPathPrefix)

-- | The typed Swagger surface's props for one request: the exact values the
-- page renders and the shell's page-enhancement descriptor reads, with every
-- URL (including the fallback's document link) applied through the request's
-- path prefix. One builder keeps the rendered page and its behavior-module
-- descriptor on the same URLs.
docsSwaggerUiProps :: RouteRequest AppRoute AppRequestContext -> SwaggerUiProps AppRoute AppRequestContext
docsSwaggerUiProps request =
  baseProps
    { swaggerUiSpecUrl = specUrl,
      swaggerUiBundleUrl = prefixed (swaggerUiBundleUrl baseProps),
      swaggerUiStylesheetUrl = prefixed (swaggerUiStylesheetUrl baseProps),
      swaggerUiModuleUrl = prefixed (swaggerUiModuleUrl baseProps),
      swaggerUiFallbackBody = swaggerUiFallback specUrl
    }
  where
    prefix = requestPathPrefix (requestContext request)
    baseProps = defaultSwaggerUiProps (requestRoute request) (requestContext request)
    specUrl = requiredSwaggerSpecUrlOrDie (prefixed "/docs/openapi.json")
    prefixed path = HarchWeb.urlPathText (HarchWeb.applyPathPrefix prefix (HarchWeb.mkUrlPath path))

-- | Build the rendered docs page for one request: the typed Swagger surface
-- with its own scoped stylesheet (the page owns its styles) and every URL
-- prefix-applied through the request context.
docsSwaggerPage :: RouteRequest AppRoute AppRequestContext -> Page AppRoute AppRequestContext
docsSwaggerPage request =
  (swaggerUiPage props)
    { HarchWeb.pageStylesheets = [swaggerUiStylesheet props]
    }
  where
    props = docsSwaggerUiProps request
