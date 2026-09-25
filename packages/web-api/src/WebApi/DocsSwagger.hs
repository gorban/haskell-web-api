{-# LANGUAGE OverloadedStrings #-}

-- | The @/docs@ Swagger UI page as an ordinary typed application surface
-- (AHI-4E slice 6). The page renders complete SSR with a script-free
-- fallback and an enhancement mount; the pinned self-hosted renderer,
-- stylesheet, and behavior module are the harch-web-openapi package's
-- assets, mounted under @/docs/assets@. Every URL is applied through the
-- application's path prefix at render time, so a deployed prefix cannot
-- leave the page pointing at root-absolute asset locations.
module WebApi.DocsSwagger (docsSwaggerPage) where

import HarchWeb (Page, RouteRequest (..))
import HarchWeb qualified
import HarchWeb.OpenApi.Swagger
  ( SwaggerUiProps (..),
    defaultSwaggerUiProps,
    requiredSwaggerSpecUrlOrDie,
    swaggerUiPage,
    swaggerUiStylesheet,
  )
import WebApi.Route (AppRequestContext, AppRoute, requestPathPrefix)

-- | Build the rendered docs page for one request: the typed Swagger surface
-- with its own scoped stylesheet (the page owns its styles) and every URL
-- prefix-applied through the request context.
docsSwaggerPage :: RouteRequest AppRoute AppRequestContext -> Page AppRoute AppRequestContext
docsSwaggerPage request =
  let prefix = requestPathPrefix (requestContext request)
      baseProps = defaultSwaggerUiProps (requestRoute request) (requestContext request)
      props =
        baseProps
          { swaggerUiSpecUrl = requiredSwaggerSpecUrlOrDie (prefixed "/docs/openapi.json"),
            swaggerUiBundleUrl = prefixed (swaggerUiBundleUrl baseProps),
            swaggerUiStylesheetUrl = prefixed (swaggerUiStylesheetUrl baseProps),
            swaggerUiModuleUrl = prefixed (swaggerUiModuleUrl baseProps)
          }
      prefixed path = HarchWeb.urlPathText (HarchWeb.applyPathPrefix prefix (HarchWeb.mkUrlPath path))
   in (swaggerUiPage props)
        { HarchWeb.pageStylesheets = [swaggerUiStylesheet props]
        }
