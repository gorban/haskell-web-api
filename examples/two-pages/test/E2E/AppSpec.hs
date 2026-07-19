{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App (buildApplication)
import HarchWeb (LocalTestServer (..), withLocalTestServer)

spec =
  describe "two-page real-browser behavior" $ do
    it "enhances navigation without replacing complete SSR documents" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit homeUrl
            assertText (byRole Heading) (`shouldBe` "Home")
            assertAttribute (css "link[rel='stylesheet']") "href" (`shouldBe` Just "/assets/two-pages.css")
            assertAttribute (css "section[data-page='home']") "class" (`shouldBe` Just "harch-home-root")
            click (byRole Link `named` "Go to the second page")
            assertUrl (`shouldBe` secondUrl)
            assertText (byRole Heading) (`shouldBe` "Second")
            assertMetrics $ \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
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
            assertUrl (`shouldBe` homeUrl)
            assertText (byRole Heading) (`shouldBe` "Home")
            historyForward
            assertUrl (`shouldBe` secondUrl)
            assertText (byRole Heading) (`shouldBe` "Second")
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
            assertUrl (`shouldBe` homeUrl)
            assertValue emailField (`shouldBe` "ada@example")
            assertMetrics $ \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 0}|])
            releaseRequestsMatching "**/assets/navigation.js"
            assertText (css "#subscription-result") (`shouldBe` "Enter a valid email address.")
            assertFocused emailField (`shouldBe` True)
            assertValue emailField (`shouldBe` "ada@example")
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertText (css "#subscription-result") (`shouldBe` "Thanks. Your subscription request is ready.")
            assertMetrics $ \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 2}|])
          )
          `shouldReturn` Right ()

    it "keeps reload and script-disabled navigation fully server rendered" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit secondUrl
            reload
            assertText (byRole Heading) (`shouldBe` "Second")
            visitWithoutScripts homeUrl
            click (byRole Link `named` "Go to the second page")
            assertUrl (`shouldBe` secondUrl)
            assertText (byRole Heading) (`shouldBe` "Second")
            assertMetrics $ \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
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
