{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

-- Adapter contract tests: one immediate locator query supplies the complete
-- record, and only the Haskell assertion loop waits for a different result.
spec = beforeAll requirePlaywrightBrowserConfig $ parallel $ describe "browser element snapshot contract" $ do
  it "matches absence immediately even with a long assertion timeout" $ \browser ->
    runBrowserSpec browser {browserTimeoutMilliseconds = 60000} do
      visit fixtureUrl
      _ <- runPageScript "window.snapshotStarted = performance.now(); true"
      assertAllObserved do
        $([|observeElement (css "#missing")|] `matchesPattern` [p|Nothing|])
      elapsed <- runPageScript "performance.now() - window.snapshotStarted"
      -- A generous bound distinguishes immediate success from waiting out the
      -- 60-second assertion timeout without measuring browser startup speed.
      assertEventually (pure elapsed) (`shouldSatisfy` \case Aeson.Number milliseconds -> milliseconds < 10000; _ -> False)

  it "reads exact text, control value, visibility and focus as one record" $ \browser ->
    runBrowserSpec browser do
      visit fixtureUrl
      fill (css "input") "typed value"
      assertAllObserved do
        $([|observeElement (byRole Heading `named` "Profile")|] `matchesPattern` [p|Just ElementSnapshot {elementText = "Account details", elementValue = Nothing, elementVisible = True, elementFocused = False}|])
        $([|observeElement (css "input")|] `matchesPattern` [p|Just ElementSnapshot {elementText = "", elementValue = Just "typed value", elementVisible = True, elementFocused = True}|])
        $([|observeElement (css "textarea")|] `matchesPattern` [p|Just ElementSnapshot {elementValue = Just "initial text"}|])
        $([|observeElement (css "select")|] `matchesPattern` [p|Just ElementSnapshot {elementValue = Just "chosen"}|])
        $([|observeElement (css "#empty")|] `matchesPattern` [p|Just ElementSnapshot {elementText = "", elementValue = Nothing}|])
        $([|observeElement (css "#nested")|] `matchesPattern` [p|Just ElementSnapshot {elementText = "  beforehiddenafter  "}|])
      press (css "input") "ControlOrMeta+A"
      press (css "input") "Backspace"
      assertAllObserved do
        $([|observeElement (css "input")|] `matchesPattern` [p|Just ElementSnapshot {elementValue = Just "", elementFocused = True}|])

  it "keeps hidden elements present and agrees with Playwright visibility" $ \browser ->
    runBrowserSpec browser do
      visit fixtureUrl
      assertAllObserved do
        $([|observeElement (css "#hidden")|] `matchesPattern` [p|Just ElementSnapshot {elementText = "hidden", elementVisible = False}|])
        mapM_
          ( \selector ->
              ((,) <$> observeElement (css selector) <*> isVisible (css selector)) `matches` \(snapshot, visible) ->
                fmap elementVisible snapshot `shouldBe` Just visible
          )
          ["#hidden", "#invisible", "#transparent", "#zero", "#contents", "#contents-text", "#contents-empty", "#closed-details", "#offscreen"]

  it "observes focus inside a shadow root" $ \browser ->
    runBrowserSpec browser do
      visit fixtureUrl
      _ <- runPageScript "document.querySelector('#shadow-host').attachShadow({ mode: 'open' }).innerHTML = '<input id=shadow-input>'; true"
      fill (css "#shadow-input") "shadow value"
      assertAllObserved do
        $([|observeElement (css "#shadow-input")|] `matchesPattern` [p|Just ElementSnapshot {elementValue = Just "shadow value", elementFocused = True}|])

  it "rejects ambiguous locators instead of treating them as absence" $ \browser -> do
    result <- runBrowserScenario browser do
      visit fixtureUrl
      assertAllObserved do
        observeElement (css ".duplicate") `shouldEqual` Nothing
    result `shouldSatisfy` \case
      Left (BrowserCommandFailed _ message _) -> all (`Text.isInfixOf` Text.pack message) ["ambiguous snapshot locator", ".duplicate", "2 elements"]
      _ -> False

  it "keeps invalid selectors as command failures" $ \browser -> do
    result <- runBrowserScenario browser do
      visit fixtureUrl
      assertAllObserved do
        observeElement (css "[") `shouldEqual` Nothing
    result `shouldSatisfy` \case
      Left BrowserCommandFailed {} -> True
      _ -> False

fixtureUrl :: Text.Text
fixtureUrl = "data:text/html,<h1 aria-label='Profile'>Account details</h1><input><textarea>initial text</textarea><select><option value='chosen'>Chosen label</option></select><div id='empty'></div><div id='nested'>  before<span id='hidden' style='display:none'>hidden</span>after  </div><div id='invisible' style='visibility:hidden'>invisible</div><div id='transparent' style='opacity:0'>transparent</div><div id='zero' style='width:0;height:0'></div><div id='contents' style='display:contents'><span>child</span></div><div id='contents-text' style='display:contents'>text</div><div id='contents-empty' style='display:contents'><!-- comment --></div><details><summary>Closed</summary><div id='closed-details'>hidden detail</div></details><div id='offscreen' style='position:absolute;left:-10000px'>offscreen</div><div id='shadow-host'></div><p class='duplicate'>same</p><p class='duplicate'>same</p>"
