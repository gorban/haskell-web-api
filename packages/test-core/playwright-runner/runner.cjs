#!/usr/bin/env node
// Haskell owns the scenario DSL. This process is deliberately only a thin protocol adapter to
// Playwright's official Node client.
const fs = require('node:fs');

async function main() {
  const [requestPath, responsePath] = process.argv.slice(2);
  if (!requestPath || !responsePath) throw new Error('expected request and response paths');
  const requestLines = fs.readFileSync(requestPath, 'utf8').split(/\r?\n/).filter(Boolean);
  const configuration = Object.fromEntries(requestLines
    .filter((line) => line.startsWith('headless\t') || line.startsWith('keep-open-on-failure\t'))
    .map((line) => line.split('\t'))
    .map((parts) => [parts[0], parts[1]]));
  const actions = requestLines.filter((line) => line.startsWith('action\t')).map((line) => line.split('\t'));
  let chromium;
  try {
    ({ chromium } = require('playwright'));
  } catch (error) {
    return writeError(responsePath, `Playwright is unavailable: ${error.message}`);
  }
  const browser = await chromium.launch({ headless: configuration.headless !== 'false' });
  let context = await browser.newContext();
  let page = await context.newPage();
  let mutationCount = 0;
  const observe = (targetPage) => targetPage.on('request', (request) => {
    if (request.headers()['x-harch-action']) mutationCount += 1;
  });
  observe(page);
  try {
    for (const parts of actions) await applyAction(parts, { browser, get page() { return page; }, setPage: async (scriptsEnabled) => {
      await context.close();
      context = await browser.newContext({ javaScriptEnabled: scriptsEnabled });
      page = await context.newPage();
      observe(page);
    }, mutationCount: () => mutationCount });
    fs.writeFileSync(responsePath, 'ok\n');
  } catch (error) {
    writeError(responsePath, error && error.message ? error.message : String(error));
  } finally {
    if (configuration['keep-open-on-failure'] !== 'true') await browser.close();
  }
}

async function applyAction(parts, state) {
  const action = parts[1];
  const value = () => parts.slice(2).join('\t');
  switch (action) {
    // The SSR document is actionable at commit: the inline capture kernel is already in the head,
    // while deferred component modules may deliberately still be loading.
    case 'visit-url': return state.page.goto(parts[2], { waitUntil: 'commit', timeout: 10000 });
    case 'visit-url-without-scripts': await state.setPage(false); return state.page.goto(parts[2], { waitUntil: 'commit', timeout: 10000 });
    case 'reload-page': return state.page.reload({ waitUntil: 'commit', timeout: 10000 });
    case 'click-link-with-text': return state.page.getByRole('link', { name: value(), exact: true }).click();
    case 'click-selector': return state.page.locator(value()).click();
    case 'fill-field': return state.page.locator(parts[2]).fill(parts.slice(3).join('\t'));
    case 'submit-form': return state.page.locator(value()).evaluate((form) => form.requestSubmit());
    case 'history-back': return state.page.goBack({ waitUntil: 'domcontentloaded' });
    case 'history-forward': return state.page.goForward({ waitUntil: 'domcontentloaded' });
    case 'assert-text-equals': {
      const actual = await state.page.locator(parts[2]).textContent();
      return assertEqual(actual || '', parts.slice(3).join('\t'), parts[2]);
    }
    case 'assert-field-value': {
      const actual = await state.page.locator(parts[2]).inputValue();
      return assertEqual(actual, parts.slice(3).join('\t'), parts[2]);
    }
    case 'assert-focused-selector': {
      const focused = await state.page.locator(value()).evaluate((element) => document.activeElement === element);
      if (!focused) throw new Error(`Expected focus on ${value()}`);
      return;
    }
    case 'assert-mutation-count': return assertEqual(String(state.mutationCount()), parts[2], 'mutation count');
    default: throw new Error(`Unsupported action: ${action}`);
  }
}

function assertEqual(actual, expected, subject) {
  if (actual !== expected) throw new Error(`Expected ${subject} to equal ${expected}, but found ${actual}`);
}

function writeError(responsePath, message) {
  fs.writeFileSync(responsePath, `error\t${String(message).replace(/[\r\n\t]/g, ' ')}\n`);
}

main().catch((error) => {
  const responsePath = process.argv[3];
  if (responsePath) writeError(responsePath, error && error.message ? error.message : String(error));
});
