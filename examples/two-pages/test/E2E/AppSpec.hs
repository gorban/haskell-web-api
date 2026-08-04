{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App (buildApplication)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb (LocalTestServer (..), withLocalTestServer)

spec =
  describe "two-page real-browser behavior" $ do
    it "enhances navigation without replacing complete SSR documents" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit homeUrl
            assertAll
              ((,,) <$> textContent (byRole Heading `named` "Home") <*> attributeValue (css "link[rel='stylesheet']") "href" <*> attributeValue (css "section[data-page='home']") "class")
              ( \(heading, stylesheetHref, homeClass) ->
                  (heading `shouldBe` "Home")
                    :| [ stylesheetHref `shouldBe` Just "/assets/two-pages.css",
                         homeClass `shouldBe` Just "harch-home-root"
                       ]
              )
            click (byRole Link `named` "Go to the second page")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "Second") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` secondUrl)
                    :| [ heading `shouldBe` "Second",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "uses the enhanced path for Back and Forward" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit homeUrl
            click (byRole Link `named` "Go to the second page")
            assertUrl (`shouldBe` secondUrl)
            historyBack
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "Home"))
              (\(url, heading) -> (url `shouldBe` homeUrl) :| [heading `shouldBe` "Home"])
            historyForward
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "Second"))
              (\(url, heading) -> (url `shouldBe` secondUrl) :| [heading `shouldBe` "Second"])
          )
          `shouldReturn` Right ()

    it "captures a submitted control before the deferred module loads, then patches its SSR region" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            fill emailField "ada@example"
            submit subscriptionForm
            assertAll
              ((,,) <$> currentUrl <*> inputValue emailField <*> browserMetrics)
              ( \(url, email, metrics) ->
                  (url `shouldBe` homeUrl)
                    :| [ email `shouldBe` "ada@example",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 0}|])
                       ]
              )
            releaseRequestsMatching "**/assets/navigation.js"
            assertAll
              ((,,) <$> textContent (css "#subscription-result") <*> isFocused emailField <*> inputValue emailField)
              ( \(result, focused, email) ->
                  (result `shouldBe` "Enter a valid email address.")
                    :| [focused `shouldBe` True, email `shouldBe` "ada@example"]
              )
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertAll
              ((,) <$> textContent (css "#subscription-result") <*> browserMetrics)
              ( \(result, metrics) ->
                  (result `shouldBe` "Thanks. Your subscription request is ready.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 2}|])]
              )
          )
          `shouldReturn` Right ()

    it "does not perform a native submission for the default exclusive client action when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        ( runBrowserScenario browser $ do
            visitWithoutScripts homeUrl
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertAll
              ((,,) <$> currentUrl <*> inputValue emailField <*> browserMetrics)
              ( \(url, email, metrics) ->
                  (url `shouldBe` homeUrl)
                    :| [ email `shouldBe` "ada@example.com",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "keeps a permanently blocked action visibly recoverable until the user cancels it" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = css "[data-harch-action-status]"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertEventually (textContent actionStatus) (`shouldBe` "Still waiting for this action to be handled.")
            click (byRole Button `named` "Cancel action")
            assertAll
              ((,) <$> textContent actionStatus <*> browserMetrics)
              ( \(status, metrics) ->
                  (status `shouldBe` "Action cancelled.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])]
              )
          )
          `shouldReturn` Right ()

    it "keeps reload and script-disabled navigation fully server rendered" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit secondUrl
            reload
            assertText (byRole Heading `named` "Second") (`shouldBe` "Second")
            visitWithoutScripts homeUrl
            click (byRole Link `named` "Go to the second page")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "Second") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` secondUrl)
                    :| [ heading `shouldBe` "Second",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "preserves the server-rendered live status until the optional EventSource module updates it" $
      withBrowserAndServer $ \browser server -> do
        let liveDataUrl = localServerBaseUrl server <> "/live-data"
        ( runBrowserScenario browser $ do
            visitWithoutScripts liveDataUrl
            assertAll
              ((,) <$> textContent (byRole Heading `named` "Live updates") <*> textContent (css "#live-data-status"))
              ( \(heading, status) ->
                  (heading `shouldBe` "Live updates")
                    :| [status `shouldBe` "Waiting for an update."]
              )
            visit liveDataUrl
            assertText (css "#live-data-status") (`shouldBe` "The live update arrived.")
          )
          `shouldReturn` Right ()

withBrowserAndServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withBrowserAndServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withLocalTestServer buildApplication (action browser)
