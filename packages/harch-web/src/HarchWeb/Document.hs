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
    buildNavigation,
    buildPageShell,
    defaultCaptureKernel,
    defaultCaptureKernelByteBudget,
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
import HarchWeb.Markup (Html, renderHtml, text)
import HarchWeb.PathPrefix (applyPathPrefix, mkPathPrefix, mkUrlPath, urlPathText)
import HarchWeb.Routing (RouteCodec, routeHref)
import HarchWeb.StaticAssets (AssetPath (..), Stylesheet (..))
import System.IO (IOMode (ReadMode), withBinaryFile)

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
  urlPathText (applyPathPrefix (mkPathPrefix pathPrefix) (mkUrlPath (navigationRuntimePath runtime)))

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
      "      throw new Error('cross-origin captured action');",
      "    }",
      "    const csrfCookie = document.cookie.split('; ').find((cookie) => cookie.startsWith('harch-csrf='));",
      "    if (!csrfCookie) {",
      "      throw new Error('missing CSRF token');",
      "    }",
      "    const csrfToken = decodeURIComponent(csrfCookie.slice('harch-csrf='.length));",
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
      "    applyActionResponse(await response.json());",
      "  }",
      "",
      "  function registerCapturedActionHandler() {",
      "    const captureKernel = window.__harchCaptureKernel;",
      "    if (!captureKernel) {",
      "      return;",
      "    }",
      "    captureKernel.register(captureKernel.eventTypes.Submit, async (capturedAction, settlement) => {",
      "      try {",
      "        await dispatchCapturedAction(capturedAction);",
      "        settlement.completed();",
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
      "  registerCapturedActionHandler();",
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
    documentMainContent :: Html,
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
      "  const capturedActions = new Map();",
      "  const handlers = new Map();",
      "  let nextActionId = 1;",
      "  let nextClaimId = 1;",
      "  let beforeUnloadInstalled = false;",
      "  const controlSelector = '[data-harch-control]';",
      "  const actionSelector = 'form[data-harch-action=\"true\"]';",
      "  const CapturedEvent = Object.freeze({ Submit: 'submit' });",
      "  const actionState = Object.freeze({ Pending: 'pending', Claimed: 'claimed', Completed: 'completed', Recoverable: 'recoverable' });",
      "  const statusFor = (control) => control.querySelector('[data-harch-action-status]');",
      "  const retryFor = (control) => control.querySelector('[data-harch-action-retry]');",
      "  const cancelFor = (control) => control.querySelector('[data-harch-action-cancel]');",
      "  const permitsRetry = (entry) => entry.control.dataset.harchActionCapabilities.split(',').some((capability) => capability === 'handler-safe-retry' || capability === 'idempotent-mutation-retry');",
      "  const setStatus = (entry, state, message) => {",
      "    entry.state = state;",
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
      "      cancel.hidden = state !== actionState.Pending && state !== actionState.Claimed && state !== actionState.Recoverable;",
      "    }",
      "  };",
      "  const requiresLeaveConfirmation = (entry) => entry.control.dataset.harchActionCapabilities.split(',').includes('conditional-leave-confirmation');",
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
      "  const settle = (entry, claimId, outcome) => {",
      "    if (entry.claimId !== claimId || entry.state !== actionState.Claimed) {",
      "      return false;",
      "    }",
      "    window.clearTimeout(entry.livenessTimer);",
      "    entry.claimId = null;",
      "    if (outcome === 'completed') {",
      "      setStatus(entry, actionState.Completed, entry.control.dataset.harchActionCompletedCopy || 'Completed.');",
      "      capturedActions.delete(entry.id);",
      "    } else {",
      "      setStatus(entry, actionState.Recoverable, entry.control.dataset.harchActionRecoverableCopy || 'This action needs your attention.');",
      "    }",
      "    updateBeforeUnload();",
      "    return true;",
      "  };",
      "  const startLivenessTimer = (entry) => {",
      "    window.clearTimeout(entry.livenessTimer);",
      "    entry.livenessTimer = window.setTimeout(() => {",
      "      if (entry.state === actionState.Pending || entry.state === actionState.Claimed) {",
      "        setStatus(entry, entry.state, entry.control.dataset.harchActionDelayedCopy || 'Still waiting for this action to be handled.');",
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
      "      const settlement = Object.freeze({ completed: () => settle(entry, claimId, 'completed'), recoverable: () => settle(entry, claimId, 'recoverable') });",
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
      "    return { type: CapturedEvent.Submit, action: target.dataset.harchActionPath || target.action, method: target.dataset.harchActionMethod || target.method, idempotencyKey: target.dataset.harchActionIdempotencyKey, fields };",
      "  };",
      "  const capturedEventHandlers = { [CapturedEvent.Submit]: captureSubmit };",
      "  const capture = (event) => {",
      "    const capturedEvent = capturedEventHandlers[event.type]?.(event);",
      "    if (capturedEvent) {",
      "      const actionId = String(nextActionId++);",
      "      const entry = { id: actionId, type: capturedEvent.type, envelope: capturedEvent, control: event.target, claimId: null, state: actionState.Pending, livenessTimer: null };",
      "      capturedActions.set(actionId, entry);",
      "      entry.control.dataset.harchActionId = actionId;",
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
      "    window.clearTimeout(entry.livenessTimer);",
      "    setStatus(entry, 'cancelled', entry.control.dataset.harchActionCancelledCopy || 'Action cancelled.');",
      "    capturedActions.delete(actionId);",
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
      "  document.addEventListener('error', (event) => {",
      "    const script = event.target instanceof HTMLScriptElement ? event.target : null;",
      "    if (!script || script.type !== 'module') { return; }",
      "    capturedActions.forEach((entry) => {",
      "      if (entry.state === actionState.Pending || entry.state === actionState.Claimed) {",
      "        window.clearTimeout(entry.livenessTimer);",
      "        entry.claimId = null;",
      "        setStatus(entry, actionState.Recoverable, entry.control.dataset.harchActionRecoverableCopy || 'This action needs your attention.');",
      "      }",
      "    });",
      "    updateBeforeUnload();",
      "  }, true);",
      "  window.__harchCaptureKernel = {",
      "    register: (eventType, handler) => { handlers.set(eventType, handler); claimPending(eventType); },",
      "    cancel,",
      "    retry,",
      "    eventTypes: CapturedEvent,",
      "  };",
      "})();"
    ]

-- | The first-fold kernel is limited to 12 KiB of rendered UTF-8 source. It
-- owns capture and lifecycle coordination only; transport and patch behavior
-- remain in deferred modules.
defaultCaptureKernelByteBudget :: Int
defaultCaptureKernelByteBudget = 12288

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

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildPageShell codec shell page =
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
renderDocumentWithNonce runtimeNonce document =
  Text.concat
    [ "<!DOCTYPE html><html><head><title>",
      renderHtml (text (documentTitle document)),
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
      renderHtml (text (documentMainId document)),
      "\"",
      renderAttributes (documentMainAttributes document <> renderBootstrapHookAttributes (documentBootstrapHooks document)),
      ">",
      renderHtml (documentMainContent document),
      "</main></body></html>"
    ]

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
          "<link rel=\"stylesheet\" href=\"" <> renderHtml (text assetPath) <> "\">"
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
