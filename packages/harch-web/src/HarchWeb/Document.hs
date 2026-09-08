{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed SSR document authoring and rendering.
module HarchWeb.Document
  ( Document (..),
    HtmlAttribute (..),
    LiveRegion (..),
    NavigationAnnouncement (..),
    NavigationFocusTarget (..),
    NavigationItem (..),
    NavigationLifecycle (..),
    NavigationRuntime (..),
    NavigationSkipLink (..),
    Page (..),
    PageShell (..),
    ResolvedNavigationItem (..),
    RuntimeAsset (..),
    RuntimeDescriptor (..),
    RuntimeNonce,
    ViewportPolicy (..),
    buildNavigation,
    buildPageShell,
    defaultCaptureKernel,
    defaultCaptureKernelByteBudget,
    defaultCaptureKernelScript,
    defaultNavigationRuntime,
    responsiveViewport,
    defaultNavigationRuntimeScript,
    defaultDialogRuntime,
    defaultDialogRuntimeScript,
    generateRuntimeNonce,
    liveRegionAttributes,
    mainNavigationLifecycle,
    navigationRuntimeScriptSource,
    runtimeAssetScriptSource,
    renderDocumentForTests,
    testRuntimeNonce,
    renderDocumentWithNonce,
    renderDocumentWithNonceAndActionCsrf,
    runtimeNonceValue,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Localization (Locale, localeText)
import HarchWeb.Markup (ElementId, Html, elementIdText, renderHtml, safeUrlText, text)
import HarchWeb.PathPrefix (PathPrefix, applyPathPrefix, mkUrlPath, urlPathText)
import HarchWeb.Routing (RouteCodec, routeHref)
import HarchWeb.StaticAssets (AssetPath (..), CssClass, Stylesheet (..), cssClassText)

data Page route context = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageContext :: context,
    pageBody :: Html,
    pageBootstrapHooks :: [Text]
  }

instance (Eq route, Eq context) => Eq (Page route context) where
  left == right =
    pageTitle left == pageTitle right
      && pageRoute left == pageRoute right
      && pageContext left == pageContext right
      && renderHtml (pageBody left) == renderHtml (pageBody right)
      && pageBootstrapHooks left == pageBootstrapHooks right

instance (Show route, Show context) => Show (Page route context) where
  showsPrec precedence page =
    showParen (precedence > 10) $
      showString "Page {pageTitle = "
        . shows (pageTitle page)
        . showString ", pageRoute = "
        . shows (pageRoute page)
        . showString ", pageContext = "
        . shows (pageContext page)
        . showString ", pageBody = "
        . shows (renderHtml (pageBody page))
        . showString ", pageBootstrapHooks = "
        . shows (pageBootstrapHooks page)
        . showString "}"

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

-- | A framework-authored JavaScript module served by the existing early
-- response boundary and declared independently in a shell. Applications can
-- replace an asset value or omit its descriptor without a dialog-specific
-- server field or a second static-file policy.
data RuntimeAsset = RuntimeAsset
  { runtimeAssetName :: Text,
    runtimeAssetPath :: Text,
    runtimeAssetScript :: Text
  }
  deriving (Eq, Show)

-- | A native skip link rendered before every other focusable body control.
-- The label is page-specific so applications can localize it at their normal
-- shell boundary. Styling remains an optional typed class owned by the app.
data NavigationSkipLink = NavigationSkipLink
  { skipLinkLabel :: Text,
    skipLinkClass :: Maybe CssClass
  }
  deriving (Eq, Show)

-- | The element that receives focus after a compatible enhanced navigation.
-- 'FocusMainLandmark' is the framework-owned safe default. An application may
-- instead name an element inside the replaced navigation or main region; it
-- then owns rendering that element as programmatically focusable on every
-- compatible page. A missing or out-of-region target makes the response
-- incompatible and preserves the hard-navigation fallback.
data NavigationFocusTarget
  = FocusMainLandmark
  | FocusElement ElementId
  deriving (Eq, Show)

-- | The text copied into the fixed polite route-status node after navigation.
-- Role, politeness, and atomicity are deliberately not configurable because
-- contradictory live-region semantics would break the lifecycle contract.
data NavigationAnnouncement
  = AnnounceDocumentTitle
  | AnnounceElementText ElementId
  deriving (Eq, Show)

-- | Declarative DOM bindings for the existing navigation runtime.
--
-- Decision (AHI-8, 2026-08-31): Harch extends the existing 'PageShell' and
-- 'NavigationRuntime' boundaries instead of adding a second SPA dispatcher.
-- This ordinary value is the pluggable server-rendered adapter: applications
-- can select a typed focus target and announcement source, localize or omit
-- the native skip link, and style the fixed status node. The supplied
-- 'mainNavigationLifecycle' is the accessible default. Applications needing a
-- genuinely different completion algorithm keep using the already-replaceable
-- 'NavigationRuntime'; Harch does not accept selector strings, JavaScript
-- callbacks, or arbitrary live-region attributes through this value.
data NavigationLifecycle = NavigationLifecycle
  { navigationSkipLink :: Maybe NavigationSkipLink,
    navigationFocusTarget :: NavigationFocusTarget,
    navigationAnnouncement :: NavigationAnnouncement,
    navigationStatusClass :: Maybe CssClass
  }
  deriving (Eq, Show)

-- | The recommended lifecycle adapter: a native skip link to the stable main
-- landmark, main focus after replacement, and one document-title announcement.
mainNavigationLifecycle :: Text -> NavigationLifecycle
mainNavigationLifecycle label =
  NavigationLifecycle
    { navigationSkipLink = Just NavigationSkipLink {skipLinkLabel = label, skipLinkClass = Nothing},
      navigationFocusTarget = FocusMainLandmark,
      navigationAnnouncement = AnnounceDocumentTitle,
      navigationStatusClass = Nothing
    }

-- | The default progressive-navigation asset. It is document-facing authoring
-- data: servers only decide how to deliver the declared path. The runtime
-- tracks the last document it rendered and leaves same-document history
-- changes to the browser. This is required for fragment links such as an
-- accessible error-summary target: a fragment @popstate@ must preserve the
-- current region patch and use native focus/scroll behavior rather than fetch
-- and replace the document.
defaultNavigationRuntime :: NavigationRuntime
defaultNavigationRuntime =
  NavigationRuntime
    { navigationRuntimePath = "/assets/navigation.js",
      navigationRuntimeScript = defaultNavigationRuntimeScript
    }

navigationRuntimeScriptSource :: PathPrefix -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  urlPathText (applyPathPrefix pathPrefix (mkUrlPath (navigationRuntimePath runtime)))

runtimeAssetScriptSource :: PathPrefix -> RuntimeAsset -> Text
runtimeAssetScriptSource pathPrefix runtimeAsset =
  urlPathText (applyPathPrefix pathPrefix (mkUrlPath (runtimeAssetPath runtimeAsset)))

-- | The replaceable native-dialog adapter. It consumes the capture kernel's
-- bounded link-trigger envelope, relies on @showModal()@ for top-layer and
-- background semantics, owns focus containment/dismissal/restoration, and
-- suppresses restoration when navigation replaces the invoker's document
-- region.
defaultDialogRuntime :: RuntimeAsset
defaultDialogRuntime =
  RuntimeAsset
    { runtimeAssetName = "harch-dialog",
      runtimeAssetPath = "/assets/dialog.js",
      runtimeAssetScript = defaultDialogRuntimeScript
    }

defaultDialogRuntimeScript :: Text
defaultDialogRuntimeScript =
  Text.unlines
    [ "(() => {",
      "  const dialogSelector = 'dialog[data-harch-dialog-root]';",
      "  const closeSelector = '[data-harch-dialog-close]';",
      "  const focusableSelector = 'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex=\"-1\"])';",
      "  const invokers = new WeakMap();",
      "  const suppressedRestoration = new WeakSet();",
      "",
      "  function focusInitial(dialog) {",
      "    const initialId = dialog.dataset.harchDialogInitialFocusId;",
      "    const initial = initialId ? dialog.querySelector(`#${CSS.escape(initialId)}`) : null;",
      "    (initial || dialog.querySelector(closeSelector))?.focus();",
      "  }",
      "",
      "  function closeDialog(dialog, restoreFocus) {",
      "    if (!restoreFocus) {",
      "      suppressedRestoration.add(dialog);",
      "    }",
      "    if (dialog.open) {",
      "      dialog.close();",
      "    }",
      "  }",
      "",
      "  function openCapturedDialog(capturedDialog, settlement) {",
      "    const dialog = document.getElementById(capturedDialog.dialogId);",
      "    const trigger = capturedDialog.trigger;",
      "    if (!(dialog instanceof HTMLDialogElement) || !dialog.matches(dialogSelector) || !(trigger instanceof HTMLAnchorElement) || !trigger.isConnected) {",
      "      settlement.recoverable();",
      "      return;",
      "    }",
      "",
      "    document.querySelectorAll(`${dialogSelector}[open]`).forEach((openDialog) => {",
      "      if (openDialog !== dialog) {",
      "        closeDialog(openDialog, true);",
      "      }",
      "    });",
      "    invokers.set(dialog, trigger);",
      "    trigger.setAttribute('aria-expanded', 'true');",
      "    if (!dialog.open) {",
      "      dialog.showModal();",
      "    }",
      "    focusInitial(dialog);",
      "    settlement.completed();",
      "  }",
      "",
      "  document.addEventListener('click', (event) => {",
      "    const closeControl = event.target instanceof Element ? event.target.closest(closeSelector) : null;",
      "    const dialog = closeControl ? closeControl.closest(dialogSelector) : null;",
      "    if (dialog instanceof HTMLDialogElement) {",
      "      event.preventDefault();",
      "      closeDialog(dialog, true);",
      "    }",
      "  });",
      "",
      "  document.addEventListener('cancel', (event) => {",
      "    const dialog = event.target;",
      "    if (dialog instanceof HTMLDialogElement && dialog.matches(dialogSelector)) {",
      "      event.preventDefault();",
      "      closeDialog(dialog, true);",
      "    }",
      "  }, true);",
      "",
      "  document.addEventListener('keydown', (event) => {",
      "    const dialog = event.target instanceof Element ? event.target.closest(`${dialogSelector}[open]`) : null;",
      "    if (!(dialog instanceof HTMLDialogElement) || event.key !== 'Tab') {",
      "      return;",
      "    }",
      "    const focusable = Array.from(dialog.querySelectorAll(focusableSelector)).filter((element) => !element.hidden);",
      "    if (focusable.length === 0) {",
      "      event.preventDefault();",
      "      return;",
      "    }",
      "    const first = focusable[0];",
      "    const last = focusable[focusable.length - 1];",
      "    if (event.shiftKey && document.activeElement === first) {",
      "      event.preventDefault();",
      "      last.focus();",
      "    } else if (!event.shiftKey && document.activeElement === last) {",
      "      event.preventDefault();",
      "      first.focus();",
      "    }",
      "  });",
      "",
      "  document.addEventListener('close', (event) => {",
      "    const dialog = event.target;",
      "    if (!(dialog instanceof HTMLDialogElement) || !dialog.matches(dialogSelector)) {",
      "      return;",
      "    }",
      "    const trigger = invokers.get(dialog);",
      "    trigger?.setAttribute('aria-expanded', 'false');",
      "    if (suppressedRestoration.has(dialog)) {",
      "      suppressedRestoration.delete(dialog);",
      "    } else if (trigger?.isConnected) {",
      "      trigger.focus();",
      "    }",
      "  }, true);",
      "",
      "  document.addEventListener('harch:navigation-before-replace', () => {",
      "    document.querySelectorAll(`${dialogSelector}[open]`).forEach((dialog) => closeDialog(dialog, false));",
      "  });",
      "",
      "  const captureKernel = window.__harchCaptureKernel;",
      "  if (captureKernel) {",
      "    captureKernel.register(captureKernel.eventTypes.DialogTrigger, openCapturedDialog);",
      "  }",
      "})();"
    ]

defaultNavigationRuntimeScript :: Text
defaultNavigationRuntimeScript =
  Text.unlines
    [ "(() => {",
      "  const pageLinkSelector = 'a[data-page-link=\"true\"]';",
      "  const navigationRegionSelector = 'nav[data-navigation-region=\"primary\"]';",
      "  const navigationContentSelector = 'main[data-navigation-content=\"true\"]';",
      "  const navigationSkipLinkSelector = 'a[data-navigation-skip-link=\"true\"]';",
      "  const navigationStatusSelector = '[data-navigation-route-status=\"true\"]';",
      "  const stylesheetSelector = 'link[data-harch-stylesheet=\"true\"]';",
      "  const pageEnhancementSelector = 'script[data-harch-page-enhancement]';",
      "  let activeNavigation = null;",
      "  let nextNavigationId = 1;",
      "  let renderedDocumentUrl = new URL(window.location.href);",
      "  let activePageEnhancementDisposers = [];",
      "",
      "  function readPageEnhancementDescriptors(rootDocument, baseUrl) {",
      "    const descriptors = Array.from(rootDocument.querySelectorAll(pageEnhancementSelector), (element) => ({ name: element.dataset.harchPageEnhancement, source: element.getAttribute('src') }));",
      "    const names = new Set();",
      "    for (const descriptor of descriptors) {",
      "      if (!descriptor.name || !descriptor.source || names.has(descriptor.name)) { return null; }",
      "      let sourceUrl;",
      "      try { sourceUrl = new URL(descriptor.source, baseUrl); } catch (_error) { return null; }",
      "      if (sourceUrl.origin !== window.location.origin) { return null; }",
      "      descriptor.source = sourceUrl.href;",
      "      names.add(descriptor.name);",
      "    }",
      "    return descriptors;",
      "  }",
      "",
      "  async function loadPageEnhancementModules(descriptors) {",
      "    try {",
      "      const modules = await Promise.all(descriptors.map(async (descriptor) => ({ descriptor, module: await import(descriptor.source) })));",
      "      return modules.every(({ module }) => typeof module.setupPageEnhancement === 'function') ? modules : null;",
      "    } catch (_error) {",
      "      return null;",
      "    }",
      "  }",
      "",
      "  function disposePageEnhancementDisposers(disposers) {",
      "    try {",
      "      disposers.forEach((dispose) => dispose());",
      "      return true;",
      "    } catch (_error) {",
      "      return false;",
      "    }",
      "  }",
      "",
      "  function disposePageEnhancements() {",
      "    const disposers = activePageEnhancementDisposers;",
      "    activePageEnhancementDisposers = [];",
      "    return disposePageEnhancementDisposers(disposers);",
      "  }",
      "",
      "  function installPageEnhancements(modules, navigationContent) {",
      "    const disposers = [];",
      "    try {",
      "      modules.forEach(({ module }) => {",
      "        const dispose = module.setupPageEnhancement(navigationContent);",
      "        if (dispose !== undefined && typeof dispose !== 'function') { throw new Error('invalid page enhancement disposer'); }",
      "        if (dispose) { disposers.push(dispose); }",
      "      });",
      "      activePageEnhancementDisposers = disposers;",
      "      return true;",
      "    } catch (_error) {",
      "      disposePageEnhancementDisposers(disposers);",
      "      return false;",
      "    }",
      "  }",
      "",
      "  function readStylesheetSources(rootDocument, baseUrl) {",
      "    const sources = Array.from(rootDocument.querySelectorAll(stylesheetSelector), (element) => element.getAttribute('href'));",
      "    const resolved = [];",
      "    for (const source of sources) {",
      "      if (!source) { return null; }",
      "      let sourceUrl;",
      "      try { sourceUrl = new URL(source, baseUrl); } catch (_error) { return null; }",
      "      if (sourceUrl.origin !== window.location.origin || resolved.includes(sourceUrl.href)) { return null; }",
      "      resolved.push(sourceUrl.href);",
      "    }",
      "    return resolved;",
      "  }",
      "",
      "  function reconcileStylesheets(nextSources) {",
      "    const currentStylesheets = Array.from(document.querySelectorAll(stylesheetSelector));",
      "    const currentSources = new Set(currentStylesheets.map((element) => element.href));",
      "    currentStylesheets.forEach((element) => { if (!nextSources.includes(element.href)) { element.remove(); } });",
      "    nextSources.forEach((source) => {",
      "      if (!currentSources.has(source)) {",
      "        const stylesheet = document.createElement('link');",
      "        stylesheet.rel = 'stylesheet';",
      "        stylesheet.dataset.harchStylesheet = 'true';",
      "        stylesheet.href = source;",
      "        document.head.append(stylesheet);",
      "      }",
      "    });",
      "  }",
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
      "    if (!actionResponse.navigation && actionResponse.focusId) {",
      "      document.getElementById(actionResponse.focusId)?.focus();",
      "    }",
      "    return actionResponse.navigation || null;",
      "  }",
      "",
      "  async function dispatchCapturedAction(capturedAction) {",
      "    const actionUrl = new URL(capturedAction.action, window.location.href);",
      "    if (actionUrl.origin !== window.location.origin) {",
      "      throw new Error('cross-origin captured action');",
      "    }",
      "    const csrfToken = document.body.dataset.harchCsrfToken;",
      "    if (!csrfToken) {",
      "      throw new Error('missing CSRF token');",
      "    }",
      "    const body = new URLSearchParams([...(capturedAction.fields || []), ['_harch_csrf', csrfToken]]).toString();",
      "    const response = await window.fetch(actionUrl, {",
      "      method: capturedAction.method || 'POST',",
      "      credentials: 'same-origin',",
      "      headers: {",
      "        'Accept': 'application/json',",
      "        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8',",
      "        'X-Harch-Action': '1',",
      "        ...(capturedAction.idempotencyKey ? { 'Idempotency-Key': capturedAction.idempotencyKey } : {}),",
      "      },",
      "      body,",
      "    });",
      "    if (!response.ok && response.status >= 500) {",
      "      throw new Error('server action failure');",
      "    }",
      "    const actionResponse = await response.json();",
      "    if (response.status === 401 && response.headers.get('X-Harch-Action-Reauthenticate') === 'required') {",
      "      return { reauthenticationRequired: true };",
      "    }",
      "    return { actionResponse, reauthenticationRequired: false, responseSucceeded: response.ok };",
      "  }",
      "",
      "  async function navigateActionResponse(navigation) {",
      "    if (!navigation || (navigation.historyMode !== 'push' && navigation.historyMode !== 'replace') || typeof navigation.href !== 'string') {",
      "      throw new Error('invalid action navigation');",
      "    }",
      "    const targetUrl = new URL(navigation.href, window.location.href);",
      "    if (targetUrl.origin !== window.location.origin) {",
      "      throw new Error('cross-origin action navigation');",
      "    }",
      "    await navigateTo(targetUrl.href, navigation.historyMode === 'push');",
      "  }",
      "",
      "  async function refreshPageSecurityForRetainedAction() {",
      "    const response = await window.fetch(window.location.href, { credentials: 'same-origin' });",
      "    if (!response.ok) { return false; }",
      "    const refreshedDocument = new DOMParser().parseFromString(await response.text(), 'text/html');",
      "    const csrfToken = refreshedDocument.body?.dataset.harchCsrfToken;",
      "    if (!csrfToken) { return false; }",
      "    document.body.dataset.harchCsrfToken = csrfToken;",
      "    return true;",
      "  }",
      "",
      "  function registerCapturedActionHandler() {",
      "    const captureKernel = window.__harchCaptureKernel;",
      "    if (!captureKernel) {",
      "      return;",
      "    }",
      "    captureKernel.refreshPageSecurityForRetainedAction = refreshPageSecurityForRetainedAction;",
      "    captureKernel.register(captureKernel.eventTypes.Submit, async (capturedAction, settlement) => {",
      "      try {",
      "        const outcome = await dispatchCapturedAction(capturedAction);",
      "        if (outcome.reauthenticationRequired) {",
      "          const actionId = settlement.retainForReauthentication();",
      "          if (actionId !== null) {",
      "            document.dispatchEvent(new CustomEvent('harch:action-reauthentication-required', { detail: { actionId } }));",
      "          }",
      "          return;",
      "        }",
      "        const navigation = applyActionResponse(outcome.actionResponse);",
      "        if (!outcome.responseSucceeded) {",
      "          settlement.recoverable();",
      "          return;",
      "        }",
      "        if (!settlement.completed()) {",
      "          return;",
      "        }",
      "        const completion = outcome.responseSucceeded && capturedAction.completion === 'reauthentication-continuation'",
      "          ? new CustomEvent('harch:action-reauthentication-completed', { cancelable: true, detail: { navigation } })",
      "          : null;",
      "        if (completion) { document.dispatchEvent(completion); }",
      "        if (navigation && (!completion || !completion.defaultPrevented)) {",
      "          await navigateActionResponse(navigation);",
      "        }",
      "      } catch (_error) {",
      "        settlement.recoverable();",
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
      "  function isRenderedDocument(targetUrl) {",
      "    const absoluteUrl = toAbsoluteUrl(targetUrl);",
      "    return absoluteUrl.origin === renderedDocumentUrl.origin",
      "      && absoluteUrl.pathname === renderedDocumentUrl.pathname",
      "      && absoluteUrl.search === renderedDocumentUrl.search;",
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
      "  function syncElementAttributes(currentElement, nextElement) {",
      "    const nextAttributes = new Map(Array.from(nextElement.attributes, (attribute) => [attribute.name, attribute.value]));",
      "    Array.from(currentElement.attributes).forEach((attribute) => {",
      "      if (!nextAttributes.has(attribute.name)) {",
      "        currentElement.removeAttribute(attribute.name);",
      "      }",
      "    });",
      "    nextAttributes.forEach((value, name) => currentElement.setAttribute(name, value));",
      "  }",
      "",
      "  function readNavigationLifecycle(rootDocument, navigationRegion, navigationContent) {",
      "    const status = rootDocument.querySelector(navigationStatusSelector);",
      "    const skipLink = rootDocument.querySelector(navigationSkipLinkSelector);",
      "    if (!status) {",
      "      return skipLink ? null : { status: null, skipLink: null, focusTargetId: null, announcementText: null };",
      "    }",
      "",
      "    const focusTargetId = status.dataset.navigationFocusTargetId;",
      "    const announcementSource = status.dataset.navigationAnnouncementSource;",
      "    const focusTarget = focusTargetId ? rootDocument.getElementById(focusTargetId) : null;",
      "    const focusIsReplaced = focusTarget && (focusTarget === navigationRegion || focusTarget === navigationContent || navigationRegion.contains(focusTarget) || navigationContent.contains(focusTarget));",
      "    if (!focusIsReplaced) {",
      "      return null;",
      "    }",
      "",
      "    let announcementText = null;",
      "    if (announcementSource === 'document-title') {",
      "      announcementText = rootDocument.title;",
      "    } else if (announcementSource === 'element-text') {",
      "      const sourceId = status.dataset.navigationAnnouncementSourceId;",
      "      const sourceElement = sourceId ? rootDocument.getElementById(sourceId) : null;",
      "      announcementText = sourceElement ? sourceElement.textContent : null;",
      "    }",
      "",
      "    if (announcementText === null) {",
      "      return null;",
      "    }",
      "",
      "    return { status, skipLink, focusTargetId, announcementText };",
      "  }",
      "",
      "  function applyNavigationLifecycle(lifecycle) {",
      "    if (!lifecycle.status) {",
      "      return;",
      "    }",
      "    const focusTarget = document.getElementById(lifecycle.focusTargetId);",
      "    focusTarget.focus({ preventScroll: true });",
      "    focusTarget.scrollIntoView({ block: 'start' });",
      "    lifecycle.status.replaceChildren(lifecycle.announcementText);",
      "  }",
      "",
      "  async function applyFetchedDocument(responseText, finalUrl, shouldPushState, isCurrentNavigation) {",
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
      "    const nextEnhancementDescriptors = readPageEnhancementDescriptors(parsedDocument, finalUrl);",
      "    const nextStylesheetSources = readStylesheetSources(parsedDocument, finalUrl);",
      "    if (!nextEnhancementDescriptors || !nextStylesheetSources) { return false; }",
      "    const nextEnhancementModules = await loadPageEnhancementModules(nextEnhancementDescriptors);",
      "    if (!isCurrentNavigation()) { return false; }",
      "    if (!nextEnhancementModules) { return false; }",
      "",
      "    const nextLifecycle = readNavigationLifecycle(parsedDocument, nextNavigationRegion, nextNavigationContent);",
      "    const currentLifecycle = readNavigationLifecycle(document, currentNavigationRegion, currentNavigationContent);",
      "    if (!nextLifecycle || !currentLifecycle || Boolean(nextLifecycle.status) !== Boolean(currentLifecycle.status) || Boolean(nextLifecycle.skipLink) !== Boolean(currentLifecycle.skipLink)) {",
      "      return false;",
      "    }",
      "",
      "    document.dispatchEvent(new CustomEvent('harch:navigation-before-replace'));",
      "    if (!disposePageEnhancements()) { return false; }",
      "    reconcileStylesheets(nextStylesheetSources);",
      "    if (currentLifecycle.skipLink) {",
      "      currentLifecycle.skipLink.replaceWith(nextLifecycle.skipLink);",
      "    }",
      "    currentNavigationRegion.replaceWith(nextNavigationRegion);",
      "    currentNavigationContent.replaceWith(nextNavigationContent);",
      "    syncBodyAttributes(parsedDocument.body);",
      "    document.documentElement.lang = parsedDocument.documentElement.lang;",
      "    if (currentLifecycle.status) {",
      "      syncElementAttributes(currentLifecycle.status, nextLifecycle.status);",
      "      nextLifecycle.status = currentLifecycle.status;",
      "    }",
      "    document.title = nextTitle.textContent || document.title;",
      "    renderedDocumentUrl = finalUrl;",
      "",
      "    if (shouldPushState) {",
      "      window.history.pushState({ path: finalUrl.href }, '', finalUrl.href);",
      "    } else if (window.location.href !== finalUrl.href) {",
      "      window.history.replaceState({ path: finalUrl.href }, '', finalUrl.href);",
      "    }",
      "",
      "    if (!installPageEnhancements(nextEnhancementModules, nextNavigationContent)) { return false; }",
      "    applyNavigationLifecycle(nextLifecycle);",
      "    return true;",
      "  }",
      "",
      "  async function navigateTo(targetUrl, shouldPushState) {",
      "    document.dispatchEvent(new CustomEvent('harch:navigation-start'));",
      "    const navigationId = nextNavigationId++;",
      "    const abortController = new AbortController();",
      "    if (activeNavigation) {",
      "      activeNavigation.abortController.abort();",
      "    }",
      "    activeNavigation = { navigationId, abortController };",
      "    const isCurrentNavigation = () => activeNavigation && activeNavigation.navigationId === navigationId;",
      "",
      "    try {",
      "      const response = await window.fetch(targetUrl, {",
      "        credentials: 'same-origin',",
      "        signal: abortController.signal,",
      "        headers: {",
      "          'X-Requested-With': 'tiny-navigation',",
      "        },",
      "      });",
      "",
      "      if (!isCurrentNavigation()) {",
      "        return;",
      "      }",
      "",
      "      if (!response.ok) {",
      "        window.location.assign(targetUrl);",
      "        return;",
      "      }",
      "",
      "      let finalUrl;",
      "      try {",
      "        finalUrl = new URL(response.url);",
      "      } catch (_error) {",
      "        window.location.assign(targetUrl);",
      "        return;",
      "      }",
      "      if (finalUrl.origin !== window.location.origin) {",
      "        window.location.assign(targetUrl);",
      "        return;",
      "      }",
      "",
      "      const responseText = await response.text();",
      "      if (!isCurrentNavigation()) {",
      "        return;",
      "      }",
      "      const applied = await applyFetchedDocument(responseText, finalUrl, shouldPushState, isCurrentNavigation);",
      "      if (isCurrentNavigation() && !applied) {",
      "        window.location.assign(targetUrl);",
      "      }",
      "    } catch (error) {",
      "      if (isCurrentNavigation() && error.name !== 'AbortError') {",
      "        window.location.assign(targetUrl);",
      "      }",
      "    } finally {",
      "      if (isCurrentNavigation()) {",
      "        activeNavigation = null;",
      "      }",
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
      "    if (isRenderedDocument(window.location.href)) {",
      "      return;",
      "    }",
      "",
      "    void navigateTo(window.location.href, false);",
      "  }",
      "",
      "  async function installInitialPageEnhancements() {",
      "    const navigationContent = document.querySelector(navigationContentSelector);",
      "    const descriptors = readPageEnhancementDescriptors(document, window.location.href);",
      "    if (!navigationContent || !descriptors) { return; }",
      "    const modules = await loadPageEnhancementModules(descriptors);",
      "    if (modules && document.querySelector(navigationContentSelector) === navigationContent) {",
      "      installPageEnhancements(modules, navigationContent);",
      "    }",
      "  }",
      "",
      "  document.addEventListener('click', handleDocumentClick);",
      "  window.addEventListener('popstate', handlePopState);",
      "  registerCapturedActionHandler();",
      "  void installInitialPageEnhancements();",
      "})();"
    ]

-- | Runtime assets are declared before document rendering so the server can
-- apply the correct CSP policy without inspecting rendered HTML.
--
-- 'PageEnhancementModule' is the deliberately narrow declaration for behavior
-- that belongs to the currently mounted page.  Its same-origin ES module must
-- export @setupPageEnhancement :: Element -> (() -> void) | void@.  The one
-- navigation runtime imports that declared source, invokes setup only after
-- mounting the destination main region, and calls the returned disposer before
-- replacing that region again.  It never evaluates script text from a fetched
-- document.  A missing, duplicate, cross-origin, or failed enhancement is an
-- incompatible enhanced navigation and deliberately falls back to the native
-- SSR document instead of silently omitting behavior.
data RuntimeDescriptor
  = InlineBootstrap
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  | DeferredModule
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  | PageEnhancementModule
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  deriving (Eq, Show)

-- | An opaque, single-response CSP capability. It intentionally has no 'Eq'
-- instance: callers must pass the generated nonce to rendering and headers,
-- never compare or reuse it as application data.
newtype RuntimeNonce = RuntimeNonce
  { runtimeNonceValue :: Text
  }
  deriving (Show)

data ResolvedNavigationItem route = ResolvedNavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route,
    navigationHref :: Text,
    navigationIsActive :: Bool
  }
  deriving (Eq, Show)

data Document route = Document
  { documentTitle :: Text,
    -- | Application-resolved language for the root document.  Harch renders
    -- this only on @html@ and preserves any language declaration that a child
    -- component deliberately owns.
    documentLanguage :: Locale,
    documentBodyAttributes :: [HtmlAttribute],
    documentNavigationAttributes :: [HtmlAttribute],
    documentNavigation :: [ResolvedNavigationItem route],
    documentMainId :: ElementId,
    documentMainAttributes :: [HtmlAttribute],
    documentMainContent :: Html,
    documentBootstrapHooks :: [Text],
    documentNavigationLifecycle :: Maybe NavigationLifecycle,
    documentStylesheets :: [Stylesheet],
    documentViewportPolicy :: ViewportPolicy,
    documentRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

-- | Closed complete-document viewport policy. 'ResponsiveViewport' emits the
-- standard device-width, initial-scale-one declaration and deliberately omits
-- maximum-scale, minimum-scale, and user-scalable restrictions, preserving
-- user zoom. This extends the existing complete-document boundary rather than
-- admitting arbitrary application head markup.
data ViewportPolicy = ResponsiveViewport
  deriving (Eq, Show)

responsiveViewport :: ViewportPolicy
responsiveViewport = ResponsiveViewport

data PageShell route context = PageShell
  { -- | The application selects this after resolving its request locale; Harch
    -- never guesses it from a URL.  An accepted enhanced navigation adopts the
    -- replacement document's root language using this same rendered value.
    shellDocumentLanguage :: Locale,
    shellBodyAttributes :: [HtmlAttribute],
    shellNavigationAttributes :: [HtmlAttribute],
    shellNavigationItems :: [NavigationItem route],
    shellMainId :: ElementId,
    shellMainAttributes :: [HtmlAttribute],
    shellNavigationLifecycle :: Maybe NavigationLifecycle,
    shellStylesheets :: [Stylesheet],
    shellRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

-- | This tiny capture-phase kernel is deliberately inline in the head. It is
-- installed before any framework control in the body can become interactive;
-- larger behavior modules consume its queue after they load.  A claimed action
-- may present patches, focus, or typed navigation only after its settlement
-- succeeds. A non-success action response may first present its typed patch,
-- but then always settles recoverably before it can complete or navigate. A
-- declared reauthentication continuation is narrower still: its
-- completion event is emitted only after a successful action response, so an
-- ordinary rejected credential patch cannot authorize or expose a retained
-- action replay.  Explicit cancellation and every enhanced-navigation start
-- invalidate outstanding claims before later transport completions can affect
-- the document.  A new capture from one control supersedes its older claim;
-- captures from distinct controls remain independent.  This cancels client
-- presentation, not a mutation the server may already have performed.
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
      "  const capturedActions = new Map();",
      "  const handlers = new Map();",
      "  let nextActionId = 1;",
      "  let nextClaimId = 1;",
      "  let beforeUnloadInstalled = false;",
      "  const controlSelector = '[data-harch-control]';",
      "  const actionSelector = 'form[data-harch-action=\"true\"]';",
      "  const CapturedEvent = Object.freeze({ Submit: 'submit', DialogTrigger: 'dialog-trigger' });",
      "  const actionState = Object.freeze({ Pending: 'pending', Claimed: 'claimed', Completed: 'completed', Recoverable: 'recoverable', Retained: 'retained', Cancelled: 'cancelled' });",
      "  const statusFor = (control) => control.querySelector('[data-harch-action-status]');",
      "  const retryFor = (control) => control.querySelector('[data-harch-action-retry]');",
      "  const cancelFor = (control) => control.querySelector('[data-harch-action-cancel]');",
      "  const isDialogEntry = (entry) => entry.type === CapturedEvent.DialogTrigger;",
      "  const permitsRetry = (entry) => entry.control.dataset.harchActionCapabilities.split(',').some((capability) => capability === 'handler-safe-retry' || capability === 'idempotent-mutation-retry');",
      "  const setStatus = (entry, state, message) => {",
      "    entry.state = state;",
      "    if (isDialogEntry(entry)) { return; }",
      "    const status = statusFor(entry.control);",
      "    if (status) {",
      "      status.dataset.harchActionState = state;",
      "      status.textContent = message;",
      "    }",
      "    entry.control.setAttribute('aria-busy', state === actionState.Pending || state === actionState.Claimed ? 'true' : 'false');",
      "    const retry = retryFor(entry.control);",
      "    if (retry) { retry.hidden = state !== actionState.Recoverable || !permitsRetry(entry); }",
      "    const cancel = cancelFor(entry.control);",
      "    if (cancel) {",
      "      cancel.hidden = state !== actionState.Pending && state !== actionState.Claimed && state !== actionState.Recoverable && state !== actionState.Retained;",
      "    }",
      "  };",
      "  const requiresLeaveConfirmation = (entry) => !isDialogEntry(entry) && entry.control.dataset.harchActionCapabilities.split(',').includes('conditional-leave-confirmation');",
      "  const retainedActionLifetime = (entry) => {",
      "    const configured = Number.parseInt(entry.control.dataset.harchActionRetentionMs || '', 10);",
      "    return Number.isSafeInteger(configured) && configured > 0 ? configured : 600000;",
      "  };",
      "  const updateBeforeUnload = () => {",
      "    const needsWarning = [...capturedActions.values()].some((entry) => requiresLeaveConfirmation(entry) && (entry.state === actionState.Pending || entry.state === actionState.Claimed || entry.state === actionState.Recoverable));",
      "    if (needsWarning && !beforeUnloadInstalled) {",
      "      window.addEventListener('beforeunload', warnBeforeUnload);",
      "      beforeUnloadInstalled = true;",
      "    } else if (!needsWarning && beforeUnloadInstalled) {",
      "      window.removeEventListener('beforeunload', warnBeforeUnload);",
      "      beforeUnloadInstalled = false;",
      "    }",
      "  };",
      "  const warnBeforeUnload = (event) => { event.preventDefault(); event.returnValue = ''; };",
      "  const followDialogFallback = (entry) => {",
      "    window.clearTimeout(entry.liveTimer);",
      "    capturedActions.delete(entry.id);",
      "    window.location.assign(entry.envelope.fallbackHref);",
      "    updateBeforeUnload();",
      "  };",
      "  const invalidate = (entry) => {",
      "    window.clearTimeout(entry.liveTimer);",
      "    window.clearTimeout(entry.reauthTimer);",
      "    entry.claimId = null;",
      "    entry.state = actionState.Cancelled;",
      "    capturedActions.delete(entry.id);",
      "  };",
      "  const supersedeControl = (control) => {",
      "    capturedActions.forEach((entry) => { if (entry.control === control) { invalidate(entry); } });",
      "  };",
      "  const settle = (entry, claimId, outcome) => {",
      "    if (entry.claimId !== claimId || entry.state !== actionState.Claimed) {",
      "      return false;",
      "    }",
      "    window.clearTimeout(entry.liveTimer);",
      "    entry.claimId = null;",
      "    if (outcome === 'completed') {",
      "      setStatus(entry, actionState.Completed, entry.control.dataset.harchActionCompletedCopy || 'Completed.');",
      "      capturedActions.delete(entry.id);",
      "    } else if (isDialogEntry(entry)) {",
      "      followDialogFallback(entry);",
      "    } else {",
      "      setStatus(entry, actionState.Recoverable, entry.control.dataset.harchActionRecoverableCopy || 'This action needs your attention.');",
      "    }",
      "    updateBeforeUnload();",
      "    return true;",
      "  };",
      "  const retainForReauthentication = (entry, claimId) => {",
      "    if (entry.claimId !== claimId || entry.state !== actionState.Claimed) { return null; }",
      "    if (entry.envelope.reauth !== 'retain-for-explicit-retry') { return null; }",
      "    window.clearTimeout(entry.liveTimer);",
      "    entry.claimId = null;",
      "    if (entry.ra > 0) {",
      "      capturedActions.delete(entry.id);",
      "      setStatus(entry, actionState.Recoverable, entry.control.dataset.harchActionRecoverableCopy || 'This action needs your attention.');",
      "      updateBeforeUnload();",
      "      return null;",
      "    }",
      "    entry.ra += 1;",
      "    setStatus(entry, actionState.Retained, entry.control.dataset.harchActionRecoverableCopy || 'Sign in to continue this action.');",
      "    entry.reauthTimer = window.setTimeout(() => {",
      "      if (entry.state === actionState.Retained) {",
      "        capturedActions.delete(entry.id);",
      "        setStatus(entry, actionState.Recoverable, entry.control.dataset.harchActionRecoverableCopy || 'This action needs your attention.');",
      "        document.dispatchEvent(new CustomEvent('harch:action-reauthentication-expired', { detail: { actionId: entry.id } }));",
      "        updateBeforeUnload();",
      "      }",
      "    }, retainedActionLifetime(entry));",
      "    updateBeforeUnload();",
      "    return entry.id;",
      "  };",
      "  const startLivenessTimer = (entry) => {",
      "    window.clearTimeout(entry.liveTimer);",
      "    entry.liveTimer = window.setTimeout(() => {",
      "      if (entry.state === actionState.Pending || entry.state === actionState.Claimed) {",
      "        if (isDialogEntry(entry)) {",
      "          followDialogFallback(entry);",
      "        } else {",
      "          setStatus(entry, entry.state, entry.control.dataset.harchActionDelayedCopy || 'Still waiting for this action to be handled.');",
      "        }",
      "      }",
      "    }, 3000);",
      "  };",
      "  const claimPending = (eventType) => {",
      "    const handler = handlers.get(eventType);",
      "    if (!handler) { return; }",
      "    capturedActions.forEach((entry) => {",
      "      if (entry.type !== eventType || entry.state !== actionState.Pending) { return; }",
      "      const claimId = String(nextClaimId++);",
      "      entry.claimId = claimId;",
      "      setStatus(entry, actionState.Claimed, entry.control.dataset.harchActionPendingCopy || 'Submitting…');",
      "      const settlement = Object.freeze({ completed: () => settle(entry, claimId, 'completed'), recoverable: () => settle(entry, claimId, 'recoverable'), retainForReauthentication: () => retainForReauthentication(entry, claimId) });",
      "      try {",
      "        Promise.resolve(handler(entry.envelope, settlement)).catch(() => settlement.recoverable());",
      "      } catch (_error) {",
      "        settlement.recoverable();",
      "      }",
      "    });",
      "  };",
      "  const retry = (actionId) => {",
      "    const entry = capturedActions.get(actionId);",
      "    if (!entry || entry.state !== actionState.Recoverable || !permitsRetry(entry) || !handlers.has(entry.type)) { return false; }",
      "    setStatus(entry, actionState.Pending, entry.control.dataset.harchActionPendingCopy || 'Submitting…');",
      "    startLivenessTimer(entry);",
      "    claimPending(entry.type);",
      "    return true;",
      "  };",
      "  const replayRetained = (actionId) => {",
      "    const entry = capturedActions.get(actionId);",
      "    if (!entry || entry.state !== actionState.Retained || !handlers.has(entry.type)) { return false; }",
      "    window.clearTimeout(entry.reauthTimer);",
      "    setStatus(entry, actionState.Pending, entry.control.dataset.harchActionPendingCopy || 'Submitting…');",
      "    startLivenessTimer(entry);",
      "    claimPending(entry.type);",
      "    return true;",
      "  };",
      "  const captureSubmit = (event) => {",
      "    const target = event.target instanceof Element ? event.target.closest(controlSelector) : null;",
      "    if (!target || !target.matches(actionSelector)) {",
      "      return null;",
      "    }",
      "    const submitter = event.submitter instanceof HTMLElement ? event.submitter : undefined;",
      "    const fields = [];",
      "    new FormData(target, submitter).forEach((value, name) => {",
      "      if (typeof value === 'string' && name !== '_harch_csrf') {",
      "        fields.push([name, value]);",
      "      }",
      "    });",
      "    return { type: CapturedEvent.Submit, action: target.dataset.harchActionPath || target.action, method: target.dataset.harchActionMethod || target.method, idempotencyKey: target.dataset.harchActionIdempotencyKey, reauth: target.dataset.harchActionReauthenticationPolicy, completion: target.dataset.harchActionCompletion, fields };",
      "  };",
      "  const captureDialogTrigger = (event) => {",
      "    if (event.defaultPrevented || event.button !== 0 || event.metaKey || event.ctrlKey || event.shiftKey || event.altKey) { return null; }",
      "    const trigger = event.target instanceof Element ? event.target.closest('a[data-harch-dialog-trigger=\"true\"]') : null;",
      "    if (!(trigger instanceof HTMLAnchorElement) || trigger.target || trigger.hasAttribute('download')) { return null; }",
      "    const dialogId = trigger.dataset.harchDialogId;",
      "    if (!dialogId) { return null; }",
      "    return { type: CapturedEvent.DialogTrigger, dialogId, fallbackHref: trigger.href, trigger };",
      "  };",
      "  const capturedEventHandlers = { click: captureDialogTrigger, submit: captureSubmit };",
      "  const capture = (event) => {",
      "    const capturedEvent = capturedEventHandlers[event.type]?.(event);",
      "    if (capturedEvent) {",
      "      const control = capturedEvent.trigger || event.target;",
      "      supersedeControl(control);",
      "      const actionId = String(nextActionId++);",
      "      const entry = { id: actionId, type: capturedEvent.type, envelope: capturedEvent, control, claimId: null, state: actionState.Pending, liveTimer: null, reauthTimer: null, ra: 0 };",
      "      capturedActions.set(actionId, entry);",
      "      if (!isDialogEntry(entry)) { entry.control.dataset.harchActionId = actionId; }",
      "      setStatus(entry, actionState.Pending, entry.control.dataset.harchActionPendingCopy || 'Submitting…');",
      "      startLivenessTimer(entry);",
      "      event.preventDefault();",
      "      updateBeforeUnload();",
      "      claimPending(capturedEvent.type);",
      "    }",
      "  };",
      "  const cancel = (actionId) => {",
      "    const entry = capturedActions.get(actionId);",
      "    if (!entry) { return false; }",
      "    invalidate(entry);",
      "    setStatus(entry, actionState.Cancelled, entry.control.dataset.harchActionCancelledCopy || 'Action cancelled.');",
      "    updateBeforeUnload();",
      "    return true;",
      "  };",
      "  Object.keys(capturedEventHandlers).forEach((eventName) => {",
      "    document.addEventListener(eventName, capture, true);",
      "  });",
      "  document.addEventListener('click', (event) => {",
      "    const retryButton = event.target instanceof Element ? event.target.closest('[data-harch-action-retry]') : null;",
      "    const retryControl = retryButton ? retryButton.closest(controlSelector) : null;",
      "    if (retryControl && retry(retryControl.dataset.harchActionId)) { event.preventDefault(); return; }",
      "    const cancelButton = event.target instanceof Element ? event.target.closest('[data-harch-action-cancel]') : null;",
      "    const control = cancelButton ? cancelButton.closest(controlSelector) : null;",
      "    if (control && cancel(control.dataset.harchActionId)) { event.preventDefault(); }",
      "  }, true);",
      "  document.addEventListener('harch:navigation-start', () => {",
      "    capturedActions.forEach((entry) => invalidate(entry));",
      "    updateBeforeUnload();",
      "  });",
      "  window.addEventListener('pagehide', () => {",
      "    capturedActions.forEach((entry) => invalidate(entry));",
      "    updateBeforeUnload();",
      "  });",
      "  document.addEventListener('error', (event) => {",
      "    const script = event.target instanceof HTMLScriptElement ? event.target : null;",
      "    if (!script || script.type !== 'module') { return; }",
      "    capturedActions.forEach((entry) => {",
      "      if (entry.state === actionState.Pending || entry.state === actionState.Claimed) {",
      "        if (isDialogEntry(entry)) {",
      "          followDialogFallback(entry);",
      "        } else {",
      "          window.clearTimeout(entry.liveTimer);",
      "          entry.claimId = null;",
      "          setStatus(entry, actionState.Recoverable, entry.control.dataset.harchActionRecoverableCopy || 'This action needs your attention.');",
      "        }",
      "      }",
      "    });",
      "    updateBeforeUnload();",
      "  }, true);",
      "  window.__harchCaptureKernel = {",
      "    register: (eventType, handler) => { handlers.set(eventType, handler); claimPending(eventType); },",
      "    cancel,",
      "    retry,",
      "    replayRetained,",
      "    eventTypes: CapturedEvent,",
      "  };",
      "})();"
    ]

-- | The first-fold kernel is limited to 12 KiB of rendered UTF-8 source. It
-- owns capture and lifecycle coordination only; transport and patch behavior
-- remain in deferred modules.
defaultCaptureKernelByteBudget :: Int
defaultCaptureKernelByteBudget = 12288

-- | Generates the 32-byte nonce that binds one complete HTML response to its
-- CSP header. Nonces are opaque so raw text cannot be interpolated into the
-- CSP or a script attribute; production rendering must obtain one here and
-- pass the same value to 'renderDocumentWithNonce' and the response headers.
generateRuntimeNonce :: IO RuntimeNonce
generateRuntimeNonce =
  RuntimeNonce . TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy 32

buildNavigation :: (Eq route) => RouteCodec route context -> Page route context -> [NavigationItem route] -> [ResolvedNavigationItem route]
buildNavigation codec page =
  map
    ( \NavigationItem {navigationLabel = itemLabel, navigationRoute = itemRoute} ->
        ResolvedNavigationItem
          { navigationLabel = itemLabel,
            navigationRoute = itemRoute,
            navigationHref = safeUrlText (routeHref codec (pageContext page) itemRoute),
            navigationIsActive = pageRoute page == itemRoute
          }
    )

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildPageShell codec shell page =
  Document
    { documentTitle = pageTitle page,
      documentLanguage = shellDocumentLanguage shell,
      documentBodyAttributes = shellBodyAttributes shell,
      documentNavigationAttributes = shellNavigationAttributes shell,
      documentNavigation = buildNavigation codec page (shellNavigationItems shell),
      documentMainId = shellMainId shell,
      documentMainAttributes = navigationMainAttributes (shellNavigationLifecycle shell) (shellMainAttributes shell),
      documentMainContent = pageBody page,
      documentBootstrapHooks = pageBootstrapHooks page,
      documentNavigationLifecycle = shellNavigationLifecycle shell,
      documentStylesheets = shellStylesheets shell,
      documentViewportPolicy = responsiveViewport,
      documentRuntimeDescriptors = shellRuntimeDescriptors shell
    }

-- | Renders deterministic markup for tests only. It deliberately has a
-- test-only name because its fixed nonce must never be paired with a
-- production CSP header; use 'renderDocumentWithNonce' with a freshly
-- generated 'RuntimeNonce' for every real response.
renderDocumentForTests :: Document route -> Text
renderDocumentForTests = renderDocumentWithNonce testRuntimeNonce

-- | Decision record (AS/AT/CR): this function, 'renderStylesheets',
-- 'renderNavigationItem', and 'renderRuntimeDescriptor'\'s 'DeferredModule'
-- case previously hand-concatenated several sinks (@main id@, a
-- navigation @href@/label, a stylesheet @href@, a deferred module @src@)
-- with no escaping at all, even though this same function already escapes
-- 'documentTitle' and every 'HtmlAttribute' via 'renderHtml' \/ 'text'.
-- Per the "extend an existing boundary" rule, every interpolated value at
-- those sinks now goes through the same 'renderHtml' \/ 'text' pair rather
-- than a new escaping mechanism. 'InlineBootstrap'\'s inline JavaScript
-- source is deliberately left unescaped: it is framework-authored script
-- content inside a @\<script\>@ element, not an HTML attribute or text
-- node, and HTML-escaping it would corrupt valid JavaScript rather than
-- neutralize an injection. This closes AT's confirmed
-- @X-Forwarded-Prefix@ reachability end to end (the sink now escapes
-- regardless of what the source validates) and makes README.md's existing
-- "escaping is centralized in the Html renderer" claim (CR) true again.
renderDocumentWithNonce :: RuntimeNonce -> Document route -> Text
renderDocumentWithNonce runtimeNonce = renderDocumentWithNonceAndActionCsrf runtimeNonce Nothing

-- | Render a production page with its exact already-selected CSRF submission
-- value.  The value is added only at the document-rendering boundary, never
-- retained in 'Page', a bootstrap hook, or diagnostics, so capture-phase
-- action code can submit it while the matching cookie remains @HttpOnly@.
renderDocumentWithNonceAndActionCsrf :: RuntimeNonce -> Maybe Text -> Document route -> Text
renderDocumentWithNonceAndActionCsrf runtimeNonce maybeActionCsrf document =
  Text.concat
    [ "<!DOCTYPE html><html lang=\"",
      renderHtml (text (localeText (documentLanguage document))),
      "\"><head><title>",
      renderHtml (text (documentTitle document)),
      "</title>",
      renderViewportPolicy (documentViewportPolicy document),
      renderStylesheets (documentStylesheets document),
      renderRuntimeDescriptors runtimeNonce (documentRuntimeDescriptors document),
      "</head><body",
      renderAttributes (documentBodyAttributes document <> actionCsrfAttribute maybeActionCsrf),
      ">",
      renderNavigationSkipLink document,
      "<nav",
      renderAttributes (documentNavigationAttributes document),
      ">",
      Text.concat (map renderNavigationItem (documentNavigation document)),
      "</nav><main id=\"",
      renderHtml (text (elementIdText (documentMainId document))),
      "\"",
      renderAttributes (documentMainAttributes document <> renderBootstrapHookAttributes (documentBootstrapHooks document)),
      ">",
      renderHtml (documentMainContent document),
      "</main>",
      renderNavigationStatus document,
      "</body></html>"
    ]

renderViewportPolicy :: ViewportPolicy -> Text
renderViewportPolicy ResponsiveViewport = "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"

actionCsrfAttribute :: Maybe Text -> [HtmlAttribute]
actionCsrfAttribute maybeActionCsrf =
  case maybeActionCsrf of
    Nothing -> []
    Just actionCsrf -> [HtmlAttribute "data-harch-csrf-token" actionCsrf]

navigationMainAttributes :: Maybe NavigationLifecycle -> [HtmlAttribute] -> [HtmlAttribute]
navigationMainAttributes lifecycle attributes =
  case navigationFocusTarget <$> lifecycle of
    Just FocusMainLandmark ->
      setFrameworkAttribute
        "data-navigation-focus-target"
        "true"
        (setFrameworkAttribute "tabindex" "-1" attributes)
    _ -> attributes

setFrameworkAttribute :: Text -> Text -> [HtmlAttribute] -> [HtmlAttribute]
setFrameworkAttribute name value attributes =
  filter ((/= name) . attributeName) attributes <> [HtmlAttribute name value]

renderNavigationSkipLink :: Document route -> Text
renderNavigationSkipLink document =
  case documentNavigationLifecycle document >>= navigationSkipLink of
    Nothing -> Text.empty
    Just NavigationSkipLink {skipLinkLabel = label, skipLinkClass = cssClass} ->
      Text.concat
        [ "<a href=\"#",
          renderHtml (text (navigationFocusTargetId document)),
          "\" data-navigation-skip-link=\"true\"",
          renderOptionalClass cssClass,
          ">",
          renderHtml (text label),
          "</a>"
        ]

renderNavigationStatus :: Document route -> Text
renderNavigationStatus document =
  case documentNavigationLifecycle document of
    Nothing -> Text.empty
    Just lifecycle ->
      Text.concat
        [ "<div data-navigation-route-status=\"true\" role=\"status\" aria-live=\"polite\" aria-atomic=\"true\" data-navigation-focus-target-id=\"",
          renderHtml (text (navigationFocusTargetId document)),
          "\"",
          renderAnnouncementAttributes (navigationAnnouncement lifecycle),
          renderOptionalClass (navigationStatusClass lifecycle),
          "></div>"
        ]

navigationFocusTargetId :: Document route -> Text
navigationFocusTargetId document =
  case navigationFocusTarget <$> documentNavigationLifecycle document of
    Just (FocusElement elementIdentifier) -> elementIdText elementIdentifier
    _ -> elementIdText (documentMainId document)

renderAnnouncementAttributes :: NavigationAnnouncement -> Text
renderAnnouncementAttributes announcement =
  case announcement of
    AnnounceDocumentTitle -> " data-navigation-announcement-source=\"document-title\""
    AnnounceElementText elementIdentifier ->
      Text.concat
        [ " data-navigation-announcement-source=\"element-text\" data-navigation-announcement-source-id=\"",
          renderHtml (text (elementIdText elementIdentifier)),
          "\""
        ]

renderOptionalClass :: Maybe CssClass -> Text
renderOptionalClass =
  maybe Text.empty (\cssClass -> " class=\"" <> renderHtml (text (cssClassText cssClass)) <> "\"")

testRuntimeNonce :: RuntimeNonce
testRuntimeNonce = RuntimeNonce "MDEyMzQ1Njc4OWFiY2RlZjAxMjM0NTY3ODlhYmNkZWY"

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
          "<link rel=\"stylesheet\" data-harch-stylesheet=\"true\" href=\"" <> renderHtml (text assetPath) <> "\">"
      )

renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = Text.concat . map renderAttribute

renderAttribute :: HtmlAttribute -> Text
renderAttribute attribute =
  Text.concat
    [ " ",
      attributeName attribute,
      "=\"",
      renderHtml (text (attributeValue attribute)),
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
renderNavigationItem
  ResolvedNavigationItem
    { navigationLabel = itemLabel,
      navigationHref = itemHref,
      navigationIsActive = itemIsActive
    } =
    Text.concat
      [ "<a href=\"",
        renderHtml (text itemHref),
        "\"",
        " data-page-link=\"true\"",
        if itemIsActive then " aria-current=\"page\"" else Text.empty,
        ">",
        renderHtml (text itemLabel),
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
          renderHtml (text source),
          "\" defer></script>"
        ]
    PageEnhancementModule {runtimeDescriptorName = name, runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script type=\"module\" data-harch-page-enhancement=\"",
          renderHtml (text name),
          "\" src=\"",
          renderHtml (text source),
          "\" defer></script>"
        ]
