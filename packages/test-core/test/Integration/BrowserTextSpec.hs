{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# E2E_SPEC #-}

import Data.Text qualified as Text

-- These are adapter contract tests: element text, optional attributes, and
-- locator errors must keep their distinct meanings across the Node protocol.
spec = beforeAll requirePlaywrightBrowserConfig $ parallel $ describe "browser text observation contract" $ do
  it "preserves exact element text separately from input values, accessible names and visibility" $ \browser ->
    runBrowserSpec browser do
      visit fixtureUrl
      assertAllObserved do
        css "#empty" `shouldHaveText` ""
        css "#whitespace" `shouldHaveText` "  spaced  "
        css "#nested" `shouldHaveText` "beforehiddenafter"
        css "input" `shouldHaveText` ""
        inputValue (css "input") `shouldEqual` "typed value"
        attributeValue (css "#empty") "title" `shouldEqual` Nothing
        byRole Heading `named` "Profile" `shouldHaveText` "Account details"
        isVisible (byRole Heading `named` "Profile") `shouldEqual` True
        css "#hidden" `shouldHaveText` "hidden"
        isVisible (css "#hidden") `shouldEqual` False
        isVisible (css "#missing") `shouldEqual` False

  it "fails for a missing element instead of accepting empty text" $ \browser -> do
    result <- runBrowserScenario browser {browserTimeoutMilliseconds = 1000} do
      visit fixtureUrl
      assertAllObserved $ css "#missing" `shouldHaveText` ""
    result `shouldSatisfy` \case
      Left (BrowserCommandFailed _ message _) -> "#missing" `Text.isInfixOf` Text.pack message && "Timeout" `Text.isInfixOf` Text.pack message
      _ -> False

  it "fails for ambiguous elements instead of choosing one" $ \browser -> do
    result <- runBrowserScenario browser do
      visit fixtureUrl
      assertAllObserved $ css ".duplicate" `shouldHaveText` "same"
    result `shouldSatisfy` \case
      Left (BrowserCommandFailed _ message _) -> "strict mode violation" `Text.isInfixOf` Text.pack message
      _ -> False

fixtureUrl :: Text.Text
fixtureUrl = "data:text/html,<h1 aria-label='Profile'>Account details</h1><div id='empty'></div><div id='whitespace'>  spaced  </div><div id='nested'>before<span id='hidden' style='display:none'>hidden</span>after</div><input value='typed value'><p class='duplicate'>same</p><p class='duplicate'>same</p>"
