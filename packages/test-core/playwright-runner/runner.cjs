#!/usr/bin/env node
// Haskell owns scenarios and assertions. This process only adapts a small,
// versioned command protocol to Playwright's official Node API.
const fs = require('node:fs');
const path = require('node:path');
const readline = require('node:readline');
const { chromium } = require('@playwright/test');

const state = {
  browser: null,
  context: null,
  page: null,
  config: null,
  scriptsEnabled: null,
  countHardNavigations: false,
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
    case 'reload': return requirePage().reload({ waitUntil: 'commit', timeout: timeout() });
    case 'click': return resolveLocator(request.locator).click({ timeout: timeout() });
    case 'press': return resolveLocator(request.locator).press(requireString(request.key, 'keyboard key'), { timeout: timeout() });
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
  state.context = await state.browser.newContext({ javaScriptEnabled: scriptsEnabled });
  state.scriptsEnabled = scriptsEnabled;
  await state.context.tracing.start({ screenshots: true, snapshots: true, sources: true });
  state.page = await state.context.newPage();
  state.countHardNavigations = false;
  state.metrics = emptyMetrics();
  state.blockedRequests = new Map();

  state.page.on('request', (request) => {
    const headers = request.headers();
    if (headers['x-requested-with'] === 'tiny-navigation') state.metrics.enhancedNavigationFetchCount += 1;
    if (headers['x-harch-action']) state.metrics.mutationRequestCount += 1;
    if (state.countHardNavigations && request.isNavigationRequest() && request.frame() === state.page.mainFrame()) {
      state.metrics.hardNavigationCount += 1;
    }
  });
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

async function visit(url, scriptsEnabled) {
  requireString(url, 'visit URL');
  if (!state.context || state.scriptsEnabled !== scriptsEnabled) await createContext(scriptsEnabled);
  state.countHardNavigations = false;
  await requirePage().goto(url, { waitUntil: 'commit', timeout: timeout() });
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
  const handler = (route) => new Promise((resolve, reject) => pendingRoutes.push({ route, resolve, reject }));
  state.blockedRequests.set(pattern, { handler, pendingRoutes });
  await state.context.route(pattern, handler);
  return null;
}

async function releaseRequestsMatching(pattern) {
  const blocked = state.blockedRequests.get(pattern);
  if (!blocked) throw new Error(`request pattern is not blocked: ${pattern}`);
  state.blockedRequests.delete(pattern);
  await Promise.all(blocked.pendingRoutes.map(async ({ route, resolve, reject }) => {
    try {
      await route.continue();
      resolve();
    } catch (error) {
      reject(error);
    }
  }));
  await state.context.unroute(pattern, blocked.handler);
  return null;
}

async function failBlockedRequestsMatching(pattern) {
  const blocked = state.blockedRequests.get(pattern);
  if (!blocked) throw new Error(`request pattern is not blocked: ${pattern}`);
  state.blockedRequests.delete(pattern);
  await Promise.all(blocked.pendingRoutes.map(async ({ route, resolve, reject }) => {
    try {
      await route.abort('failed');
      resolve();
    } catch (error) {
      reject(error);
    }
  }));
  await state.context.unroute(pattern, blocked.handler);
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
    case 'browserMetrics': return { ...state.metrics };
    default: throw new Error(`unsupported browser observation: ${observation.kind}`);
  }
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
      await state.page.screenshot({ path: screenshotPath, fullPage: true }).catch(() => {});
      fs.writeFileSync(htmlPath, await state.page.content(), 'utf8');
      if (failureMessage) fs.writeFileSync(path.join(runDirectory, 'failure.txt'), failureMessage, 'utf8');
      await state.context.tracing.stop({ path: tracePath }).catch(() => {});
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
  process.stdout.write(`${JSON.stringify(response)}\n`);
}

main().catch(async (error) => {
  await closeBrowser(true, error && error.message ? error.message : String(error)).catch(() => {});
  process.stderr.write(`${error && error.stack ? error.stack : String(error)}\n`);
  process.exitCode = 1;
});
