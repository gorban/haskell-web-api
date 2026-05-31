{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( buildApplication,
    twoPageServerConfig,
    twoPageSite,
  )
where

import App.Components.Layout (twoPageShell)
import App.Pages.Home (homePage)
import App.Pages.NotFound (notFoundPage)
import App.Pages.Second (secondPage)
import App.Routes (TwoPageRoute (..), routeCodec)
import qualified Data.Text as Text
import HarchWeb
  ( Application,
    Response (..),
    ResponseBody (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    RequestPolicyConfig (..),
    ServerConfig (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
  )
import HarchWeb.Site
  ( Site (..),
    SiteRoute (..),
    buildSiteApplication,
    pageSiteRoute,
    simpleSite,
  )

buildApplication :: Application TwoPageRoute ()
buildApplication = buildSiteApplication twoPageSite

twoPageSite :: Site TwoPageRoute ()
twoPageSite =
  ( simpleSite
      "two-pages-example"
      ()
      routeCodec
      twoPageShell
      [ pageSiteRoute HomeRoute (Just "Home") homePage,
        pageSiteRoute SecondRoute (Just "Second") secondPage,
        navigationScriptRoute,
        pageSiteRoute NotFoundRoute Nothing notFoundPage
      ]
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy
    }

twoPageServerConfig :: ServerConfig
twoPageServerConfig =
  ServerConfig
    { listenerConfigs =
        [ ListenerConfig
            { listenerHost = "127.0.0.1",
              listenerPort = 8080,
              listenerScheme = Http,
              listenerTls = Nothing,
              listenerAcme = Nothing
            }
        ],
      staticAssets = twoPageStaticAssets,
      requestPolicy = twoPageRequestPolicy,
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

twoPageStaticAssets :: StaticAssetsConfig
twoPageStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [],
      staticAssetContentTypes = defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Nothing
    }

twoPageRequestPolicy :: RequestPolicyConfig
twoPageRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      strictTransportSecurity = Nothing,
      trustForwardedHeaders = False,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }

navigationScriptRoute :: SiteRoute TwoPageRoute ()
navigationScriptRoute =
  SiteRoute
    { siteRouteValue = NavigationScriptRoute,
      siteRouteNavigationLabel = Nothing,
      siteRouteResponse =
        \_ ->
          pure
            ( BodyResponse
                ResponseBody
                  { responseStatus = 200,
                    responseContentType = "application/javascript; charset=utf-8",
                    responseBody = navigationScriptBody,
                    responseObservabilityAttributes = [],
                    responseLogEntries = []
                  }
            )
    }

navigationScriptBody :: Text.Text
navigationScriptBody =
  Text.unlines
    [ "(() => {",
      "  const pageLinkSelector = 'a[data-page-link=\"true\"]';",
      "  const navigationRegionSelector = 'nav[data-navigation-region=\"primary\"]';",
      "  const navigationContentSelector = 'main[data-navigation-content=\"true\"]';",
      "  let navigationInFlight = false;",
      "",
      "  function isPlainLeftClick(event) {",
      "    return event.button === 0 && !event.metaKey && !event.ctrlKey && !event.shiftKey && !event.altKey;",
      "  }",
      "",
      "  function toAbsoluteUrl(targetUrl) {",
      "    return new URL(targetUrl, window.location.href);",
      "  }",
      "",
      "  function isSameOriginNavigationLink(anchor) {",
      "    if (!anchor || anchor.target || anchor.hasAttribute('download')) {",
      "      return false;",
      "    }",
      "",
      "    const absoluteUrl = toAbsoluteUrl(anchor.href);",
      "    return absoluteUrl.origin === window.location.origin;",
      "  }",
      "",
      "  function syncBodyAttributes(nextBody) {",
      "    const currentBody = document.body;",
      "    const nextAttributes = new Map(Array.from(nextBody.attributes, (attribute) => [attribute.name, attribute.value]));",
      "",
      "    Array.from(currentBody.attributes).forEach((attribute) => {",
      "      if (!nextAttributes.has(attribute.name)) {",
      "        currentBody.removeAttribute(attribute.name);",
      "      }",
      "    });",
      "",
      "    nextAttributes.forEach((value, name) => {",
      "      currentBody.setAttribute(name, value);",
      "    });",
      "  }",
      "",
      "  function applyFetchedDocument(responseText, targetUrl, shouldPushState) {",
      "    const parsedDocument = new DOMParser().parseFromString(responseText, 'text/html');",
      "    const nextTitle = parsedDocument.querySelector('title');",
      "    const nextNavigationRegion = parsedDocument.querySelector(navigationRegionSelector);",
      "    const nextNavigationContent = parsedDocument.querySelector(navigationContentSelector);",
      "    const currentNavigationRegion = document.querySelector(navigationRegionSelector);",
      "    const currentNavigationContent = document.querySelector(navigationContentSelector);",
      "",
      "    if (!nextTitle || !nextNavigationRegion || !nextNavigationContent || !currentNavigationRegion || !currentNavigationContent) {",
      "      return false;",
      "    }",
      "",
      "    document.title = nextTitle.textContent || document.title;",
      "    currentNavigationRegion.replaceWith(nextNavigationRegion);",
      "    currentNavigationContent.replaceWith(nextNavigationContent);",
      "    syncBodyAttributes(parsedDocument.body);",
      "",
      "    if (shouldPushState) {",
      "      window.history.pushState({ path: targetUrl }, '', targetUrl);",
      "    }",
      "",
      "    return true;",
      "  }",
      "",
      "  async function navigateTo(targetUrl, shouldPushState) {",
      "    if (navigationInFlight) {",
      "      return;",
      "    }",
      "",
      "    navigationInFlight = true;",
      "",
      "    try {",
      "      const response = await window.fetch(targetUrl, {",
      "        credentials: 'same-origin',",
      "        headers: {",
      "          'X-Requested-With': 'tiny-navigation',",
      "        },",
      "      });",
      "",
      "      if (!response.ok) {",
      "        window.location.assign(targetUrl);",
      "        return;",
      "      }",
      "",
      "      const responseText = await response.text();",
      "      if (!applyFetchedDocument(responseText, targetUrl, shouldPushState)) {",
      "        window.location.assign(targetUrl);",
      "      }",
      "    } catch (_error) {",
      "      window.location.assign(targetUrl);",
      "    } finally {",
      "      navigationInFlight = false;",
      "    }",
      "  }",
      "",
      "  function handleDocumentClick(event) {",
      "    if (event.defaultPrevented || !isPlainLeftClick(event)) {",
      "      return;",
      "    }",
      "",
      "    const anchor = event.target.closest(pageLinkSelector);",
      "    if (!isSameOriginNavigationLink(anchor)) {",
      "      return;",
      "    }",
      "",
      "    event.preventDefault();",
      "    void navigateTo(anchor.href, true);",
      "  }",
      "",
      "  function handlePopState() {",
      "    void navigateTo(window.location.href, false);",
      "  }",
      "",
      "  document.addEventListener('click', handleDocumentClick);",
      "  window.addEventListener('popstate', handlePopState);",
      "})();"
    ]
