-- | The @/docs@ Swagger UI page as an ordinary typed application surface
-- (a later slice of the OpenAPI documentation and Swagger UI work). The page renders complete SSR with a script-free
-- fallback and an enhancement mount; the pinned self-hosted renderer,
-- stylesheet, and behavior module are the harch-web-openapi package's
-- assets, mounted under @/docs/assets@. Every URL is applied through the
-- application's path prefix at render time, so a deployed prefix cannot
-- leave the page pointing at root-absolute asset locations.
--
-- Decision record (web-api template authoring, 2026-09-28): the Swagger
-- behavior module is a page-owned runtime requirement, attached here beside
-- the page's stylesheet. The shared shell owns only application-wide assets;
-- 'buildPageShell' orders shell requirements before page requirements.
--
-- The Swagger page route is fixed here, so 'docsSwaggerUiProps' accepts only
-- request context. The page consumes its typed route from those props and
-- applies one prefix-aware builder to all its asset URLs.
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
    swaggerUiPageEnhancement,
    swaggerUiStylesheet,
  )
import WebApi.Route (AppRequestContext, AppRoute (DocsSwaggerRoute), requestPathPrefix)

-- | The typed Swagger surface's props for one request context: the exact
-- values the page renders and the shell's page-enhancement descriptor reads,
-- with every URL (including the fallback's document link) applied through
-- the request's path prefix. The page route is fixed by this module; one
-- builder keeps the rendered page and behavior-module descriptor on the same
-- URLs.
docsSwaggerUiProps :: AppRequestContext -> SwaggerUiProps AppRoute AppRequestContext
docsSwaggerUiProps context =
  baseProps
    { swaggerUiSpecUrl = specUrl,
      swaggerUiBundleUrl = prefixed (swaggerUiBundleUrl baseProps),
      swaggerUiStylesheetUrl = prefixed (swaggerUiStylesheetUrl baseProps),
      swaggerUiModuleUrl = prefixed (swaggerUiModuleUrl baseProps),
      swaggerUiFallbackBody = swaggerUiFallback specUrl
    }
  where
    prefix = requestPathPrefix context
    baseProps = defaultSwaggerUiProps DocsSwaggerRoute context
    specUrl = requiredSwaggerSpecUrlOrDie (prefixed "/docs/openapi.json")
    prefixed path = HarchWeb.urlPathText (HarchWeb.applyPathPrefix prefix (HarchWeb.mkUrlPath path))

-- | Build the rendered docs page for one request: the typed Swagger surface
-- with its own scoped stylesheet (the page owns its styles) and every URL
-- prefix-applied through the request context.
docsSwaggerPage :: RouteRequest AppRoute AppRequestContext -> Page AppRoute AppRequestContext
docsSwaggerPage request =
  HarchWeb.withPageRuntimeDescriptors
    (HarchWeb.withPageStylesheets (swaggerUiPage props) [swaggerUiStylesheet props])
    [swaggerUiPageEnhancement props]
  where
    props = docsSwaggerUiProps (requestContext request)
