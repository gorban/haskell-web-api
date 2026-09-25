{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The typed Swagger UI surface (AHI-4E): the ordinary SSR page at
-- @GET \/docs@ and its page-scoped stylesheet and behavior module, served
-- from the package's pinned, reviewed Swagger UI distribution
-- (@assets\/swagger-ui@, see its README for the pin, license, and CSP
-- rationale).
--
-- Decision record (AHI-4E, 2026-09-24): this surface stays an ordinary
-- application page, not a second dispatcher. The page is a plain 'Page'
-- value; its behavior module is a plain 'PageEnhancementModule' descriptor
-- consumed by the existing navigation runtime, which gives the
-- initialize/dispose lifecycle for free: the kernel imports the module once
-- per document and invokes its returned disposer before the next enhanced
-- navigation replaces the page (PR-C1). Assets ride the existing
-- 'HarchWeb.StaticAssets.StaticAssetRoot' boundary at @\/docs\/assets@, and
-- the document specification itself is the typed route from
-- "HarchWeb.OpenApi.Route".
--
-- All three pieces are replaceable without forking this module:
-- 'swaggerUiFallbackBody' swaps the script-free body renderer, the asset
-- URLs in 'SwaggerUiProps' relocate the provider, and an application that
-- wants a different UI entirely can build its own 'Page' over the same
-- 'HarchWeb.OpenApi.Provider' bytes.
--
-- Untrusted display text (descriptions, examples, response bodies) is
-- rendered only inside Swagger's own runtime DOM under the framework's
-- strict default CSP — this module's server-rendered fallback contains only
-- static text and typed link values, all through the escaping sink. The
-- specification URL stays a 'SafeUrl' end to end (typed link semantics for
-- the anchor, its text projection for the behavior module's DOM attribute).
module HarchWeb.OpenApi.Swagger
  ( SwaggerUiProps (..),
    defaultSwaggerUiProps,
    requiredSwaggerSpecUrlOrDie,
    swaggerUiPage,
    swaggerUiFallback,
    swaggerUiPageEnhancement,
    swaggerUiStylesheet,
    swaggerUiAssetsRoot,
  )
where

import Data.Text (Text)
import HarchWeb
  ( AssetPath (..),
    Html,
    Page (..),
    RuntimeDescriptor (..),
    SafeUrl,
    StaticAssetRoot (..),
    Stylesheet,
    harch,
    mkSafeUrl,
    requiredSafeUrlOrDie,
    safeUrlText,
    stylesheet,
    text,
  )
import Paths_harch_web_openapi (getDataFileName)

-- | Everything the surface needs, explicit and replaceable. The route and
-- context values are supplied by the application because only it owns its
-- closed route algebra and request context.
data SwaggerUiProps route context = SwaggerUiProps
  { swaggerUiRoute :: route,
    swaggerUiContext :: context,
    -- | Document and page title.
    swaggerUiTitle :: Text,
    -- | The typed specification endpoint's browser-visible URL.
    swaggerUiSpecUrl :: SafeUrl,
    -- | Where the pinned classic bundle is served from.
    swaggerUiBundleUrl :: Text,
    -- | Where the pinned stylesheet is served from.
    swaggerUiStylesheetUrl :: Text,
    -- | Where this package's behavior module is served from.
    swaggerUiModuleUrl :: Text,
    -- | The script-free server-rendered body; see 'swaggerUiFallback'.
    swaggerUiFallbackBody :: Html
  }

-- | The reference-default property set at the task file's default endpoints
-- (@\/docs@ page over @\/docs\/openapi.json@, assets under
-- @\/docs\/assets@). Applications relocating the asset provider override the
-- URLs alongside their own 'swaggerUiAssetsRoot' prefix.
defaultSwaggerUiProps :: route -> context -> SwaggerUiProps route context
defaultSwaggerUiProps routeValue context =
  SwaggerUiProps
    { swaggerUiRoute = routeValue,
      swaggerUiContext = context,
      swaggerUiTitle = "Documentation",
      swaggerUiSpecUrl = requiredSwaggerSpecUrlOrDie "/docs/openapi.json",
      swaggerUiBundleUrl = "/docs/assets/swagger-ui-bundle.js",
      swaggerUiStylesheetUrl = "/docs/assets/swagger-ui.css",
      swaggerUiModuleUrl = "/docs/assets/swagger-enhancement.js",
      swaggerUiFallbackBody = swaggerUiFallback (requiredSwaggerSpecUrlOrDie "/docs/openapi.json")
    }

-- | Unwrap a statically-known-valid specification URL literal, or crash
-- naming the offending value. Exported (like the template's
-- 'WebApi.Api.Endpoints.requiredApiHeaderNameOrDie') so its error rail is
-- directly testable against a genuinely invalid literal instead of only
-- through the always-valid defaults above.
requiredSwaggerSpecUrlOrDie :: Text -> SafeUrl
requiredSwaggerSpecUrlOrDie value =
  requiredSafeUrlOrDie
    ("HarchWeb.OpenApi.Swagger: invalid specification URL: " <> value)
    (mkSafeUrl value)

-- | The complete script-free body: the page is meaningful without any
-- JavaScript (the scripts-disabled acceptance case) and gives the reader a
-- direct link to the machine-readable document. The fallback text stays
-- visible until the behavior module reports readiness; the module hides it
-- and shows only the live UI, and its disposer restores it.
swaggerUiFallback :: SafeUrl -> Html
swaggerUiFallback specUrl =
  [harch|
    <div data-swagger-fallback="true">
      <p>API documentation loads interactively when scripts are enabled.</p>
      <p><a href={specUrl}>Open the OpenAPI document</a></p>
    </div>
  |]

-- | Render the ordinary SSR page. The renderer owns the enhancement mount
-- (with the only values the behavior module reads from the DOM); the
-- replaceable fallback renders beside it. No capture-time hooks: the
-- interactive surface is entirely enhancement-rendered and the fallback has
-- no controls whose events could be lost before hydration.
swaggerUiPage :: SwaggerUiProps route context -> Page route context
swaggerUiPage props =
  Page
    { pageStylesheets = [],
      pageTitle = swaggerUiTitle props,
      pageRoute = swaggerUiRoute props,
      pageContext = swaggerUiContext props,
      pageBody =
        [harch|
          <section data-page="docs">
            <h1>{text (swaggerUiTitle props)}</h1>
            {swaggerUiFallbackBody props}
            <div data-swagger-ui="true" data-swagger-spec-url={safeUrlText (swaggerUiSpecUrl props)} data-swagger-bundle-url={swaggerUiBundleUrl props}></div>
          </section>
        |],
      pageBootstrapHooks = []
    }

-- | The page-scoped behavior module descriptor for the navigation runtime.
-- The module (@assets\/swagger-ui\/swagger-enhancement.js@) exports
-- @setupPageEnhancement@ and its disposer; the kernel's same-origin rule and
-- its one-call-per-document discipline cover the rest.
swaggerUiPageEnhancement :: SwaggerUiProps route context -> RuntimeDescriptor
swaggerUiPageEnhancement props =
  PageEnhancementModule "harch-swagger-ui" (swaggerUiModuleUrl props)

-- | The page-scoped stylesheet for the pinned Swagger UI chrome.
swaggerUiStylesheet :: SwaggerUiProps route context -> Stylesheet
swaggerUiStylesheet props = stylesheet (AssetPath (swaggerUiStylesheetUrl props))

-- | The static-asset root serving the package's pinned distribution at the
-- application's chosen URL prefix (the defaults use @\/docs\/assets@).
-- Composition adds this root beside its own static roots; the provider is
-- replaceable by supplying a different root instead.
swaggerUiAssetsRoot :: Text -> IO StaticAssetRoot
swaggerUiAssetsRoot urlPrefix =
  StaticAssetRoot urlPrefix <$> getDataFileName "assets/swagger-ui"
