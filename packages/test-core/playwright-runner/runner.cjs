#!/usr/bin/env node
// Haskell owns scenarios and assertions. This process only adapts a small,
// versioned command protocol to Playwright's official Node API.
const fs = require('node:fs');
const path = require('node:path');
const readline = require('node:readline');
const { chromium } = require('@playwright/test');

let outputClosed = false;
process.stdout.on('error', (error) => {
  if (error && error.code === 'EPIPE') {
    outputClosed = true;
    return;
  }
  throw error;
});

const state = {
  browser: null,
  context: null,
  page: null,
  config: null,
  scriptsEnabled: null,
  mobileViewport: null,
  countHardNavigations: false,
  documentIdentity: null,
  documentRequestCount: 0,
  accountedDocumentRequestCount: 0,
  metrics: emptyMetrics(),
  blockedRequests: new Map(),
};

function emptyMetrics() {
  return {
    enhancedNavigationFetchCount: 0,
    hardNavigationCount: 0,
    mutationRequestCount: 0,
  };
}

async function main() {
  const lines = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });
  for await (const line of lines) {
    if (!line.trim()) continue;
    let request;
    try {
      request = JSON.parse(line);
      validateEnvelope(request);
      const value = await execute(request);
      writeResponse({ protocol: 1, id: request.id, status: 'ok', value });
      if (request.command === 'finish') return;
    } catch (error) {
      const id = request && Number.isInteger(request.id) ? request.id : -1;
      writeResponse({
        protocol: 1,
        id,
        status: 'error',
        message: error && error.message ? error.message : String(error),
        artifacts: [],
      });
      if (request && request.command === 'finish') return;
    }
  }
  await closeBrowser(false, null);
}

function validateEnvelope(request) {
  if (!request || request.protocol !== 1) throw new Error('unsupported or missing browser protocol version');
  if (!Number.isInteger(request.id)) throw new Error('browser command id must be an integer');
  if (typeof request.command !== 'string') throw new Error('browser command name must be a string');
}

async function execute(request) {
  switch (request.command) {
    case 'initialize': return initialize(request);
    case 'visit': return visit(request.url, true);
    case 'visitWithoutScripts': return visit(request.url, false);
    case 'setCookie': return setCookie(request.url, request.name, request.value);
    case 'setViewportSize': return requirePage().setViewportSize({ width: positiveInteger(request.width, 'viewport width'), height: positiveInteger(request.height, 'viewport height') });
    case 'emulateMobileViewport': return emulateMobileViewport(request.width, request.height);
    case 'reload': return requirePage().reload({ waitUntil: 'commit', timeout: timeout() });
    case 'click': return resolveLocator(request.locator).click({ timeout: timeout() });
    case 'press': return resolveLocator(request.locator).press(requireString(request.key, 'keyboard key'), { timeout: timeout() });
    case 'paste': return paste(resolveLocator(request.locator), requireString(request.value, 'paste value'));
    case 'runPageScript': return runPageScript(requireString(request.source, 'page script'));
    case 'fill': return resolveLocator(request.locator).fill(requireString(request.value, 'fill value'), { timeout: timeout() });
    case 'setInputFiles': return resolveLocator(request.locator).setInputFiles(requireString(request.filePath, 'file path'), { timeout: timeout() });
    case 'submit': return resolveLocator(request.locator).evaluate((form) => {
      if (!(form instanceof HTMLFormElement)) throw new Error('submit locator must resolve to a form');
      form.requestSubmit();
    });
    case 'historyBack': return requirePage().goBack({ waitUntil: 'commit', timeout: timeout() });
    case 'historyForward': return requirePage().goForward({ waitUntil: 'commit', timeout: timeout() });
    case 'blockRequestsMatching': return blockRequestsMatching(requireString(request.pattern, 'request pattern'));
    case 'waitForBlockedRequestsMatching': return waitForBlockedRequestsMatching(requireString(request.pattern, 'request pattern'));
    case 'waitForBlockedRequestCountMatching': return waitForBlockedRequestCountMatching(requireString(request.pattern, 'request pattern'), positiveInteger(request.count, 'blocked request count'));
    case 'releaseRequestsMatching': return releaseRequestsMatching(requireString(request.pattern, 'request pattern'));
    case 'failBlockedRequestsMatching': return failBlockedRequestsMatching(requireString(request.pattern, 'request pattern'));
    case 'observeMany': return observeMany(request.observations);
    case 'finish': return finish(request.failure);
    default: throw new Error(`unsupported browser command: ${request.command}`);
  }
}

async function initialize(request) {
  if (state.browser) throw new Error('browser runner is already initialized');
  state.config = {
    headless: request.headless !== false,
    pauseOnFailure: request.pauseOnFailure === true,
    timeoutMilliseconds: positiveInteger(request.timeoutMilliseconds, 'timeoutMilliseconds'),
    artifactDirectory: requireString(request.artifactDirectory, 'artifactDirectory'),
  };
  state.browser = await chromium.launch({ headless: state.config.headless });
  await createContext(true);
  return null;
}

async function createContext(scriptsEnabled) {
  if (state.context) {
    await state.context.tracing.stop().catch(() => {});
    await state.context.close();
  }
  const contextOptions = { javaScriptEnabled: scriptsEnabled };
  if (state.mobileViewport) {
    contextOptions.viewport = state.mobileViewport;
    contextOptions.screen = state.mobileViewport;
    contextOptions.isMobile = true;
    contextOptions.hasTouch = true;
  }
  state.context = await state.browser.newContext(contextOptions);
  state.scriptsEnabled = scriptsEnabled;
  await state.context.tracing.start({ screenshots: true, snapshots: true, sources: true });
  await state.context.exposeBinding('__testCoreRecordFetch', (_source, metrics) => {
    if (metrics.enhancedNavigation) state.metrics.enhancedNavigationFetchCount += 1;
    if (metrics.mutation) state.metrics.mutationRequestCount += 1;
  });
  await state.context.addInitScript(() => {
    window.__testCoreDocumentIdentity = Math.random().toString(36).slice(2);
    const originalFetch = window.fetch;
    window.fetch = function (...arguments_) {
      const [input, init] = arguments_;
      const headers = new Headers(init?.headers ?? (input instanceof Request ? input.headers : undefined));
      const requestedWith = headers.get('X-Requested-With')?.toLowerCase();
      const metrics = {
        enhancedNavigation: requestedWith === 'tiny-navigation',
        mutation: headers.has('X-Harch-Action'),
      };
      if (metrics.enhancedNavigation || metrics.mutation) {
        void window.__testCoreRecordFetch(metrics);
      }
      return originalFetch.apply(this, arguments_);
    };
  });
  state.page = await state.context.newPage();
  state.countHardNavigations = false;
  state.documentIdentity = null;
  state.documentRequestCount = 0;
  state.accountedDocumentRequestCount = 0;
  state.metrics = emptyMetrics();
  state.blockedRequests = new Map();

  state.page.on('request', (request) => {
    if (state.countHardNavigations && request.isNavigationRequest() && request.frame() === state.page.mainFrame()) {
      state.documentRequestCount += 1;
      state.metrics.hardNavigationCount += 1;
    }
  });

}

async function emulateMobileViewport(width, height) {
  state.mobileViewport = {
    width: positiveInteger(width, 'mobile viewport width'),
    height: positiveInteger(height, 'mobile viewport height'),
  };
  await createContext(state.scriptsEnabled === null ? true : state.scriptsEnabled);
  return null;
}

// `visit` uses Playwright's earliest navigation signal (`commit`) rather than
// `domcontentloaded` so a scenario can block a deferred module's request
// (see `blockRequestsMatching`) without deadlocking navigation itself: a
// blocked request is left permanently pending, and `domcontentloaded` would
// never fire while a deferred script is still waiting on one. That leaves a
// window, on a slow or contended runner, where a page script can run before
// the document has finished parsing and its synchronous inline scripts
// (such as the capture kernel) have executed. Wait for `readyState` to leave
// `loading` first: that is set once parsing and synchronous inline scripts
// are done, but before deferred/blocked scripts are awaited, so it cannot
// hit the same deadlock.
async function runPageScript(source) {
  const page = requirePage();
  await page.waitForFunction(() => document.readyState !== 'loading', null, { timeout: timeout() });
  return page.evaluate(source);
}

async function paste(locator, value) {
  const page = requirePage();
  const origin = new URL(page.url()).origin;
  await state.context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin });
  await page.evaluate((clipboardValue) => navigator.clipboard.writeText(clipboardValue), value);
  await locator.focus({ timeout: timeout() });
  await locator.press('Control+V', { timeout: timeout() });
  return null;
}

async function visit(url, scriptsEnabled) {
  requireString(url, 'visit URL');
  if (!state.context || state.scriptsEnabled !== scriptsEnabled) await createContext(scriptsEnabled);
  state.countHardNavigations = false;
  const page = requirePage();
  await page.goto(url, { waitUntil: 'commit', timeout: timeout() });
  state.documentIdentity = await page.evaluate(() => window.__testCoreDocumentIdentity);
  state.documentRequestCount = 0;
  state.accountedDocumentRequestCount = 0;
  state.metrics = emptyMetrics();
  state.countHardNavigations = true;
  return null;
}

async function setCookie(url, name, value) {
  requireString(url, 'cookie URL');
  requireString(name, 'cookie name');
  requireString(value, 'cookie value');
  if (!state.context) throw new Error('browser runner has not been initialized');
  let cookie = { url, name, value };
  if (name.startsWith('__Host-')) {
    cookie = { name, value, domain: new URL(url).hostname, path: '/', secure: true };
  }
  await state.context.addCookies([cookie]);
  return null;
}

async function blockRequestsMatching(pattern) {
  if (state.blockedRequests.has(pattern)) throw new Error(`request pattern is already blocked: ${pattern}`);
  const pendingRoutes = [];
  const blocked = { acceptingRequests: true, handler: null, pendingRoutes };
  blocked.handler = (route) => {
    if (!blocked.acceptingRequests) return route.continue();
    return new Promise((resolve, reject) => pendingRoutes.push({ route, resolve, reject }));
  };
  state.blockedRequests.set(pattern, blocked);
  await state.context.route(pattern, blocked.handler);
  return null;
}

async function waitForBlockedRequestsMatching(pattern) {
  return waitForBlockedRequestCountMatching(pattern, 1);
}

async function waitForBlockedRequestCountMatching(pattern, expectedCount) {
  const blocked = state.blockedRequests.get(pattern);
  if (!blocked) throw new Error(`request pattern is not blocked: ${pattern}`);
  const deadline = Date.now() + timeout();
  while (blocked.pendingRoutes.length < expectedCount) {
    if (Date.now() >= deadline) throw new Error(`timed out waiting for ${expectedCount} blocked requests matching: ${pattern}`);
    await new Promise((resolve) => setTimeout(resolve, 10));
  }
  return null;
}

async function releaseRequestsMatching(pattern) {
  const blocked = state.blockedRequests.get(pattern);
  if (!blocked) throw new Error(`request pattern is not blocked: ${pattern}`);
  blocked.acceptingRequests = false;
  state.blockedRequests.delete(pattern);
  // Keep the handler just long enough to release captured routes, but switch
  // it to pass-through first. A release can immediately cause a follow-up
  // request, which must neither wait in this queue nor lose the captured
  // route's intended continuation.
  for (const { route, resolve } of blocked.pendingRoutes) {
    try {
      const continued = route.continue();
      resolve();
      void continued.catch(() => {});
    } catch (_) {
      resolve();
    }
  }
  // The captured routes have already been continued and the handler now
  // passes any racing follow-up request through.  Route deregistration can
  // wait for a browser navigation to settle, so it must not hold the command
  // protocol (or the native-fallback path) hostage.
  void state.context.unroute(pattern, blocked.handler).catch(() => {});
  return null;
}

async function failBlockedRequestsMatching(pattern) {
  const blocked = state.blockedRequests.get(pattern);
  if (!blocked) throw new Error(`request pattern is not blocked: ${pattern}`);
  blocked.acceptingRequests = false;
  state.blockedRequests.delete(pattern);
  // See releaseRequestsMatching: later requests pass through while the
  // captured request still receives the requested failure.
  for (const { route, resolve } of blocked.pendingRoutes) {
    try {
      const aborted = route.abort('failed');
      resolve();
      void aborted.catch(() => {});
    } catch (_) {
      resolve();
    }
  }
  // As above, completion of the already-issued abort is independent of
  // Playwright finishing route deregistration.  Keep that cleanup detached
  // so the browser can take its error-driven native fallback immediately.
  void state.context.unroute(pattern, blocked.handler).catch(() => {});
  return null;
}

async function observeMany(observations) {
  if (!Array.isArray(observations)) throw new Error('observeMany requires an observations array');
  return Promise.all(observations.map(observe));
}

async function observe(observation) {
  if (!observation || typeof observation.kind !== 'string') throw new Error('invalid browser observation');
  switch (observation.kind) {
    case 'textContent': return (await resolveLocator(observation.locator).textContent({ timeout: timeout() })) || '';
    case 'inputValue': return resolveLocator(observation.locator).inputValue({ timeout: timeout() });
    case 'attributeValue': return resolveLocator(observation.locator).getAttribute(requireString(observation.attribute, 'attribute name'), { timeout: timeout() });
    case 'focused': return resolveLocator(observation.locator).evaluate((element) => document.activeElement === element);
    case 'visible': return resolveLocator(observation.locator).isVisible({ timeout: timeout() });
    case 'currentUrl': return requirePage().url();
    case 'browserMetrics': return browserMetrics();
    default: throw new Error(`unsupported browser observation: ${observation.kind}`);
  }
}

async function browserMetrics() {
  const documentIdentity = await currentDocumentIdentity();
  if (state.countHardNavigations && documentIdentity !== state.documentIdentity) {
    const requestWasObserved = state.documentRequestCount !== state.accountedDocumentRequestCount;
    state.documentIdentity = documentIdentity;
    state.accountedDocumentRequestCount = state.documentRequestCount;
    if (!requestWasObserved) state.metrics.hardNavigationCount += 1;
  }
  return { ...state.metrics };
}

async function currentDocumentIdentity() {
  const deadline = Date.now() + timeout();
  while (true) {
    try {
      return await requirePage().evaluate(() => window.__testCoreDocumentIdentity);
    } catch (error) {
      if (!isNavigationInterruption(error) || Date.now() >= deadline) throw error;
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
  }
}

function isNavigationInterruption(error) {
  return error instanceof Error && error.message.includes('Execution context was destroyed');
}

function resolveLocator(spec, root = requirePage()) {
  if (!spec || typeof spec.kind !== 'string') throw new Error('invalid locator');
  switch (spec.kind) {
    case 'role': return root.getByRole(requireString(spec.role, 'ARIA role'), spec.name == null ? {} : { name: spec.name, exact: true });
    case 'label': return root.getByLabel(requireString(spec.text, 'label text'), { exact: true });
    case 'text': return root.getByText(requireString(spec.text, 'visible text'), { exact: true });
    case 'placeholder': return root.getByPlaceholder(requireString(spec.text, 'placeholder text'), { exact: true });
    case 'altText': return root.getByAltText(requireString(spec.text, 'alternative text'), { exact: true });
    case 'title': return root.getByTitle(requireString(spec.text, 'title text'), { exact: true });
    case 'testId': return root.getByTestId(requireString(spec.text, 'test id'));
    case 'css': return root.locator(requireString(spec.text, 'CSS selector'));
    case 'within': return resolveLocator(spec.child, resolveLocator(spec.parent, root));
    case 'containingText': return resolveLocator(spec.locator, root).filter({ hasText: requireString(spec.text, 'contained text') });
    default: throw new Error(`unsupported locator kind: ${spec.kind}`);
  }
}

async function finish(failure) {
  const artifacts = await closeBrowser(failure != null, failure == null ? null : String(failure));
  return { artifacts };
}

async function closeBrowser(failed, failureMessage) {
  if (!state.browser) return [];
  const artifacts = [];
  try {
    if (failed && state.page && state.config) {
      if (state.config.pauseOnFailure && !state.config.headless) await state.page.pause();
      const runDirectory = path.resolve(state.config.artifactDirectory, `failure-${Date.now()}-${process.pid}`);
      fs.mkdirSync(runDirectory, { recursive: true });
      const screenshotPath = path.join(runDirectory, 'page.png');
      const htmlPath = path.join(runDirectory, 'page.html');
      const tracePath = path.join(runDirectory, 'trace.zip');
      await withinTimeout(state.page.screenshot({ path: screenshotPath, fullPage: true, timeout: timeout() })).catch(() => {});
      const pageHtml = await withinTimeout(state.page.content()).catch(() => null);
      if (pageHtml !== null) fs.writeFileSync(htmlPath, pageHtml, 'utf8');
      if (failureMessage) fs.writeFileSync(path.join(runDirectory, 'failure.txt'), failureMessage, 'utf8');
      await withinTimeout(state.context.tracing.stop({ path: tracePath })).catch(() => {});
      for (const artifactPath of [screenshotPath, htmlPath, tracePath]) {
        if (fs.existsSync(artifactPath)) artifacts.push(artifactPath);
      }
    } else if (state.context) {
      await state.context.tracing.stop().catch(() => {});
    }
  } finally {
    await state.browser.close().catch(() => {});
    state.browser = null;
    state.context = null;
    state.page = null;
  }
  return artifacts;
}

async function withinTimeout(operation) {
  let timer;
  try {
    return await Promise.race([
      operation,
      new Promise((_, reject) => {
        timer = setTimeout(() => reject(new Error('browser cleanup timed out')), timeout());
      }),
    ]);
  } finally {
    clearTimeout(timer);
  }
}

function requirePage() {
  if (!state.page) throw new Error('browser runner has not been initialized');
  return state.page;
}

function timeout() {
  if (!state.config) throw new Error('browser runner has not been initialized');
  return state.config.timeoutMilliseconds;
}

function requireString(value, description) {
  if (typeof value !== 'string' || value.length === 0) throw new Error(`${description} must be a non-empty string`);
  return value;
}

function positiveInteger(value, description) {
  if (!Number.isInteger(value) || value <= 0) throw new Error(`${description} must be a positive integer`);
  return value;
}

function writeResponse(response) {
  if (!outputClosed && !process.stdout.destroyed) {
    process.stdout.write(`${JSON.stringify(response)}\n`);
  }
}

main().catch(async (error) => {
  await closeBrowser(true, error && error.message ? error.message : String(error)).catch(() => {});
  process.stderr.write(`${error && error.stack ? error.stack : String(error)}\n`);
  process.exitCode = 1;
});
