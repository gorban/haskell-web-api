{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed SSR document authoring and rendering.
module HarchWeb.Document
  ( Document (..),
    HtmlAttribute (..),
    LiveRegion (..),
    NavigationItem (..),
    NavigationRuntime (..),
    Page (..),
    PageShell (..),
    ResolvedNavigationItem (..),
    RuntimeDescriptor (..),
    RuntimeNonce (..),
    buildDocument,
    buildNavigation,
    buildPageShell,
    defaultCaptureKernel,
    defaultCaptureKernelScript,
    defaultNavigationRuntime,
    defaultNavigationRuntimeScript,
    generateRuntimeNonce,
    liveRegionAttributes,
    navigationRuntimeScriptSource,
    renderDocument,
    renderDocumentWithNonce,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.PathPrefix (applyPathPrefix)
import HarchWeb.Routing (RouteCodec, routeHref)
import HarchWeb.StaticAssets (AssetPath (..), Stylesheet (..))
import System.IO (IOMode (ReadMode), withBinaryFile)

data Page route context = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageContext :: context,
    pageBody :: Text,
    pageBootstrapHooks :: [Text]
  }
  deriving (Eq, Show)

data HtmlAttribute = HtmlAttribute
  { attributeName :: Text,
    attributeValue :: Text
  }
  deriving (Eq, Show)

-- | The two live-region modes that are safe defaults for server-rendered region
-- patches. Keep the role and its announcement urgency together so action UIs do
-- not accidentally render a contradictory role/aria-live pair.
data LiveRegion
  = PoliteStatus
  | AssertiveAlert
  deriving (Eq, Show)

data NavigationItem route = NavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route
  }
  deriving (Eq, Show)

data NavigationRuntime = NavigationRuntime
  { navigationRuntimePath :: Text,
    navigationRuntimeScript :: Text
  }
  deriving (Eq, Show)

-- | The default progressive-navigation asset. It is document-facing authoring
-- data: servers only decide how to deliver the declared path.
defaultNavigationRuntime :: NavigationRuntime
defaultNavigationRuntime =
  NavigationRuntime
    { navigationRuntimePath = "/assets/navigation.js",
      navigationRuntimeScript = defaultNavigationRuntimeScript
    }

navigationRuntimeScriptSource :: Text -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  applyPathPrefix pathPrefix (navigationRuntimePath runtime)

defaultNavigationRuntimeScript :: Text
defaultNavigationRuntimeScript =
  Text.unlines
    [ "(() => {",
      "  const pageLinkSelector = 'a[data-page-link=\"true\"]';",
      "  const navigationRegionSelector = 'nav[data-navigation-region=\"primary\"]';",
      "  const navigationContentSelector = 'main[data-navigation-content=\"true\"]';",
      "  let navigationInFlight = false;",
      "",
      "  function applyActionResponse(actionResponse) {",
      "    (actionResponse.patches || []).forEach((patch) => {",
      "      const currentRegion = document.getElementById(patch.id);",
      "      if (!currentRegion || typeof patch.html !== 'string') {",
      "        return;",
      "      }",
      "      const replacementTemplate = document.createElement('template');",
      "      replacementTemplate.innerHTML = patch.html;",
      "      const replacementRegion = replacementTemplate.content.firstElementChild;",
      "      if (replacementRegion) {",
      "        currentRegion.replaceWith(replacementRegion);",
      "      }",
      "    });",
      "    if (actionResponse.focusId) {",
      "      document.getElementById(actionResponse.focusId)?.focus();",
      "    }",
      "  }",
      "",
      "  async function dispatchCapturedAction(capturedAction) {",
      "    const actionUrl = new URL(capturedAction.action, window.location.href);",
      "    if (actionUrl.origin !== window.location.origin) {",
      "      return;",
      "    }",
      "    const body = new URLSearchParams(capturedAction.fields || []).toString();",
      "    const response = await window.fetch(actionUrl, {",
      "      method: capturedAction.method || 'POST',",
      "      credentials: 'same-origin',",
      "      headers: {",
      "        'Accept': 'application/json',",
      "        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8',",
      "        'X-Harch-Action': '1',",
      "      },",
      "      body,",
      "    });",
      "    if (!response.ok && response.status >= 500) {",
      "      return;",
      "    }",
      "    applyActionResponse(await response.json());",
      "  }",
      "",
      "  function drainCapturedActions() {",
      "    const captureKernel = window.__harchCaptureKernel;",
      "    if (!captureKernel) {",
      "      return;",
      "    }",
      "    captureKernel.drain().forEach((capturedEvent) => {",
      "      if (capturedEvent.type === 'submit') {",
      "        void dispatchCapturedAction(capturedEvent);",
      "      }",
      "    });",
      "  }",
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
      "  window.addEventListener('harch:capture', drainCapturedActions);",
      "  drainCapturedActions();",
      "})();"
    ]

-- | Runtime assets are declared before document rendering so the server can
-- apply the correct CSP policy without inspecting rendered HTML.
data RuntimeDescriptor
  = InlineBootstrap
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  | DeferredModule
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  deriving (Eq, Show)

newtype RuntimeNonce = RuntimeNonce
  { runtimeNonceValue :: Text
  }
  deriving (Eq, Show)

data ResolvedNavigationItem route = ResolvedNavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route,
    navigationHref :: Text,
    navigationIsActive :: Bool
  }
  deriving (Eq, Show)

data Document route = Document
  { documentTitle :: Text,
    documentBodyAttributes :: [HtmlAttribute],
    documentNavigationAttributes :: [HtmlAttribute],
    documentNavigation :: [ResolvedNavigationItem route],
    documentMainId :: Text,
    documentMainAttributes :: [HtmlAttribute],
    documentMainContent :: Text,
    documentBootstrapHooks :: [Text],
    documentStylesheets :: [Stylesheet],
    documentRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

data PageShell route context = PageShell
  { shellBodyAttributes :: [HtmlAttribute],
    shellNavigationAttributes :: [HtmlAttribute],
    shellNavigationItems :: [NavigationItem route],
    shellMainId :: Text,
    shellMainAttributes :: [HtmlAttribute],
    shellStylesheets :: [Stylesheet],
    shellRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

-- | This tiny capture-phase kernel is deliberately inline in the head. It is
-- installed before any framework control in the body can become interactive;
-- larger behavior modules consume its queue after they load.
defaultCaptureKernel :: RuntimeDescriptor
defaultCaptureKernel =
  InlineBootstrap
    { runtimeDescriptorName = "harch-capture-kernel",
      runtimeDescriptorSource = defaultCaptureKernelScript
    }

defaultCaptureKernelScript :: Text
defaultCaptureKernelScript =
  Text.unlines
    [ "(() => {",
      "  const queuedEvents = [];",
      "  const controlSelector = '[data-harch-control]';",
      "  const actionSelector = 'form[data-harch-action=\"true\"]';",
      "  const capture = (event) => {",
      "    const target = event.target instanceof Element ? event.target.closest(controlSelector) : null;",
      "    if (target) {",
      "      if (event.type === 'submit' && target.matches(actionSelector)) {",
      "        const submitter = event.submitter instanceof HTMLElement ? event.submitter : undefined;",
      "        const fields = [];",
      "        new FormData(target, submitter).forEach((value, name) => {",
      "          if (typeof value === 'string') {",
      "            fields.push([name, value]);",
      "          }",
      "        });",
      "        queuedEvents.push({ type: 'submit', action: target.action, method: target.method, fields });",
      "        event.preventDefault();",
      "      } else {",
      "        queuedEvents.push({ event, target });",
      "      }",
      "      window.dispatchEvent(new Event('harch:capture'));",
      "    }",
      "  };",
      "  ['click', 'input', 'change', 'keydown', 'submit'].forEach((eventName) => {",
      "    document.addEventListener(eventName, capture, true);",
      "  });",
      "  window.__harchCaptureKernel = {",
      "    drain: () => queuedEvents.splice(0),",
      "  };",
      "})();"
    ]

generateRuntimeNonce :: IO RuntimeNonce
generateRuntimeNonce =
  withBinaryFile "/dev/urandom" ReadMode $ \randomHandle -> do
    randomBytes <- ByteString.hGet randomHandle 32
    pure
      ( RuntimeNonce
          (TextEncoding.decodeUtf8 (Base64Url.encode randomBytes))
      )

buildNavigation :: (Eq route) => RouteCodec route context -> Page route context -> [NavigationItem route] -> [ResolvedNavigationItem route]
buildNavigation codec page =
  map
    ( \NavigationItem {navigationLabel = itemLabel, navigationRoute = itemRoute} ->
        ResolvedNavigationItem
          { navigationLabel = itemLabel,
            navigationRoute = itemRoute,
            navigationHref = routeHref codec (pageContext page) itemRoute,
            navigationIsActive = pageRoute page == itemRoute
          }
    )

buildDocument :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildDocument codec shell page =
  Document
    { documentTitle = pageTitle page,
      documentBodyAttributes = shellBodyAttributes shell,
      documentNavigationAttributes = shellNavigationAttributes shell,
      documentNavigation = buildNavigation codec page (shellNavigationItems shell),
      documentMainId = shellMainId shell,
      documentMainAttributes = shellMainAttributes shell,
      documentMainContent = pageBody page,
      documentBootstrapHooks = pageBootstrapHooks page,
      documentStylesheets = shellStylesheets shell,
      documentRuntimeDescriptors = shellRuntimeDescriptors shell
    }

renderDocument :: Document route -> Text
renderDocument = renderDocumentWithNonce (RuntimeNonce "development-render-nonce")

renderDocumentWithNonce :: RuntimeNonce -> Document route -> Text
renderDocumentWithNonce runtimeNonce document =
  Text.concat
    [ "<html><head><title>",
      documentTitle document,
      "</title>",
      renderStylesheets (documentStylesheets document),
      renderRuntimeDescriptors runtimeNonce (documentRuntimeDescriptors document),
      "</head><body",
      renderAttributes (documentBodyAttributes document),
      "><nav",
      renderAttributes (documentNavigationAttributes document),
      ">",
      Text.concat (map renderNavigationItem (documentNavigation document)),
      "</nav><main id=\"",
      documentMainId document,
      "\"",
      renderAttributes (documentMainAttributes document <> renderBootstrapHookAttributes (documentBootstrapHooks document)),
      ">",
      documentMainContent document,
      "</main></body></html>"
    ]

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildPageShell = buildDocument

liveRegionAttributes :: LiveRegion -> [HtmlAttribute]
liveRegionAttributes liveRegion =
  case liveRegion of
    PoliteStatus -> [HtmlAttribute "role" "status", HtmlAttribute "aria-live" "polite", HtmlAttribute "aria-atomic" "true"]
    AssertiveAlert -> [HtmlAttribute "role" "alert", HtmlAttribute "aria-live" "assertive", HtmlAttribute "aria-atomic" "true"]

renderStylesheets :: [Stylesheet] -> Text
renderStylesheets =
  Text.concat
    . map
      ( \Stylesheet {stylesheetAsset = AssetPath assetPath} ->
          "<link rel=\"stylesheet\" href=\"" <> assetPath <> "\">"
      )

renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = Text.concat . map renderAttribute

renderAttribute :: HtmlAttribute -> Text
renderAttribute attribute =
  Text.concat
    [ " ",
      attributeName attribute,
      "=\"",
      attributeValue attribute,
      "\""
    ]

renderBootstrapHookAttributes :: [Text] -> [HtmlAttribute]
renderBootstrapHookAttributes bootstrapHooks =
  case bootstrapHooks of
    [] -> []
    _ ->
      [ HtmlAttribute
          { attributeName = "data-bootstrap-hooks",
            attributeValue = Text.intercalate "," bootstrapHooks
          }
      ]

renderNavigationItem :: ResolvedNavigationItem route -> Text
renderNavigationItem ResolvedNavigationItem {navigationLabel = itemLabel, navigationHref = itemHref, navigationIsActive = itemIsActive} =
  Text.concat
    [ "<a href=\"",
      itemHref,
      "\"",
      " data-page-link=\"true\"",
      if itemIsActive then " aria-current=\"page\"" else Text.empty,
      ">",
      itemLabel,
      "</a>"
    ]

renderRuntimeDescriptors :: RuntimeNonce -> [RuntimeDescriptor] -> Text
renderRuntimeDescriptors runtimeNonce =
  Text.concat . map (renderRuntimeDescriptor runtimeNonce)

renderRuntimeDescriptor :: RuntimeNonce -> RuntimeDescriptor -> Text
renderRuntimeDescriptor runtimeNonce descriptor =
  case descriptor of
    InlineBootstrap {runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script nonce=\"",
          runtimeNonceValue runtimeNonce,
          "\">",
          source,
          "</script>"
        ]
    DeferredModule {runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script type=\"module\" src=\"",
          source,
          "\" defer></script>"
        ]
