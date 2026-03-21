{-# LANGUAGE OverloadedStrings #-}

{-# E2E_SPEC #-}

import qualified Data.Text as Text
import qualified HarchWeb
import System.IO.Temp (withSystemTempDirectory)
import WebApi (buildApp)
import WebApi.Config (defaultAppConfig)

spec =
  describe "browser e2e" $ do
    it "loads the home page through a real local HTTP listener and asserts SSR content" $
      withNodeBrowserRunner $ \browserConfig ->
        HarchWeb.withLocalTestServer (buildApp defaultAppConfig) $ \localTestServer ->
          runBrowserScript
            browserConfig
            [ VisitUrl (Text.unpack (HarchWeb.localServerBaseUrl localTestServer) <> "/"),
              AssertTextEquals "[data-page-title=\"true\"]" "Home"
            ]
            `shouldReturn` Right ()

    it "keeps the same-origin click scenario authored in the browser DSL until the runtime lands" $
      HarchWeb.withLocalTestServer (buildApp defaultAppConfig) $ \localTestServer ->
        pendingWith
          ( "Need a browser-backed runner that can prove same-document navigation rather than a fresh SSR load. Intended actions: "
              <> show
                (sameOriginNavigationActions (Text.unpack (HarchWeb.localServerBaseUrl localTestServer)))
          )

withNodeBrowserRunner :: (BrowserConfig -> IO a) -> IO a
withNodeBrowserRunner action =
  withSystemTempDirectory "web-api-browser-runner" $ \tempDirectory -> do
    let scriptPath = tempDirectory <> "/web-api-browser-runner.js"
        browserConfig =
          defaultBrowserConfig
            { browserRunnerCommand = "node",
              browserRunnerArguments = [scriptPath]
            }
    writeFile scriptPath nodeBrowserRunnerSource
    action browserConfig

sameOriginNavigationActions :: String -> [BrowserAction]
sameOriginNavigationActions baseUrl =
  [ VisitUrl (baseUrl <> "/"),
    ClickLinkWithText "Browse the second page",
    AssertTextEquals "[data-page-title=\"true\"]" "Second"
  ]

nodeBrowserRunnerSource :: String
nodeBrowserRunnerSource =
  unlines
    [ "const fs = require('fs');",
      "const http = require('http');",
      "const https = require('https');",
      "const { URL } = require('url');",
      "",
      "async function main() {",
      "  const [requestPath, responsePath] = process.argv.slice(2);",
      "  const requestLines = fs.readFileSync(requestPath, 'utf8').split(/\\r?\\n/).filter(Boolean);",
      "  let currentHtml = null;",
      "",
      "  for (const line of requestLines) {",
      "    const parts = line.split('\\t');",
      "    if (parts[0] === 'headless' || parts[0] === 'keep-open-on-failure') {",
      "      continue;",
      "    }",
      "    if (parts[0] !== 'action') {",
      "      return writeError(responsePath, `Unexpected request line: ${line}`);",
      "    }",
      "",
      "    const action = parts[1];",
      "    switch (action) {",
      "      case 'visit-url':",
      "        currentHtml = await fetchText(parts[2]);",
      "        break;",
      "      case 'assert-text-equals': {",
      "        if (currentHtml === null) {",
      "          return writeError(responsePath, 'No page has been loaded yet.');",
      "        }",
      "        const selector = parts[2];",
      "        const expected = parts.slice(3).join('\\t');",
      "        const actual = extractSelectorText(currentHtml, selector);",
      "        if (actual !== expected) {",
      "          return writeError(responsePath, `Expected ${selector} to equal ${expected}, but found ${actual}`);",
      "        }",
      "        break;",
      "      }",
      "      default:",
      "        return writeError(responsePath, `Unsupported action: ${action}`);",
      "    }",
      "  }",
      "",
      "  fs.writeFileSync(responsePath, 'ok\\n');",
      "}",
      "",
      "function writeError(responsePath, message) {",
      "  fs.writeFileSync(responsePath, `error\\t${message}\\n`);",
      "}",
      "",
      "function fetchText(targetUrl) {",
      "  const parsedUrl = new URL(targetUrl);",
      "  const client = parsedUrl.protocol === 'https:' ? https : http;",
      "  return new Promise((resolve, reject) => {",
      "    const request = client.get(parsedUrl, (response) => {",
      "      let responseBody = '';",
      "      response.setEncoding('utf8');",
      "      response.on('data', (chunk) => {",
      "        responseBody += chunk;",
      "      });",
      "      response.on('end', () => {",
      "        resolve(responseBody);",
      "      });",
      "    });",
      "    request.on('error', reject);",
      "  });",
      "}",
      "",
      "function extractSelectorText(html, selector) {",
      "  switch (selector) {",
      "    case '[data-page-title=\"true\"]':",
      "      return extractPageTitle(html);",
      "    case 'title':",
      "      return extractElementText(html, /<title>([\\s\\S]*?)<\\/title>/i);",
      "    default:",
      "      throw new Error(`Unsupported selector: ${selector}`);",
      "  }",
      "}",
      "",
      "function extractPageTitle(html) {",
      "  return extractElementText(html, /<[^>]*data-page-title=\"true\"[^>]*>([\\s\\S]*?)<\\/[^>]+>/i);",
      "}",
      "",
      "function extractElementText(html, pattern) {",
      "  const match = html.match(pattern);",
      "  if (!match) {",
      "    throw new Error('Expected selector to match rendered HTML.');",
      "  }",
      "  return match[1].replace(/<[^>]+>/g, '').replace(/\\s+/g, ' ').trim();",
      "}",
      "",
      "main().catch((error) => {",
      "  const [, responsePath] = process.argv.slice(2);",
      "  if (responsePath) {",
      "    writeError(responsePath, error.message);",
      "    process.exit(0);",
      "  }",
      "  console.error(error);",
      "  process.exit(1);",
      "});"
    ]
