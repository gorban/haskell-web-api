{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import qualified Core.Config as CoreConfig
import qualified Data.Text as Text
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)

spec = do
  describe "parseConfigOverridesFile" $ do
    it "parses key value lines while ignoring blank lines and comments" $
      CoreConfig.parseConfigOverridesFile
        ( Text.unlines
            [ "# override file",
              " APP_TITLE_PREFIX = custom-app ",
              Text.empty,
              "LISTENER_0_PORT=6001"
            ]
        )
        `shouldBe` Right
          [ ("APP_TITLE_PREFIX", "custom-app"),
            ("LISTENER_0_PORT", "6001")
          ]

    it "rejects malformed override lines with the original line content" $ do
      CoreConfig.parseConfigOverridesFile
        ( Text.unlines
            [ "APP_TITLE_PREFIX=custom-app",
              "BROKEN_LINE"
            ]
        )
        `shouldBe` Left (CoreConfig.InvalidConfigOverridesLine 2 "BROKEN_LINE")
      CoreConfig.parseConfigOverridesFile "   =value"
        `shouldBe` Left (CoreConfig.InvalidConfigOverridesLine 1 "   =value")
      show (CoreConfig.InvalidConfigOverridesLine 2 "BROKEN_LINE")
        `shouldBe` "InvalidConfigOverridesLine 2 \"BROKEN_LINE\""

  describe "loadConfigOverridesFile" $ do
    it "returns no overrides when the file does not exist" $
      withSystemTempDirectory "core-config" $ \tempDirectory -> do
        CoreConfig.loadConfigOverridesFile (tempDirectory <> "/missing.overrides")
          `shouldReturn` Right []

    it "loads override entries from disk" $
      withSystemTempFile "runtime.overrides" $ \overridesPath overridesHandle -> do
        hPutStr overridesHandle "APP_TITLE_PREFIX=loaded-from-file\nLISTENER_0_PORT=5100\n"
        hClose overridesHandle
        CoreConfig.loadConfigOverridesFile overridesPath
          `shouldReturn` Right
            [ ("APP_TITLE_PREFIX", "loaded-from-file"),
              ("LISTENER_0_PORT", "5100")
            ]

  describe "lookupConfigValue" $ do
    it "prefers environment overrides over local overrides over committed defaults" $ do
      let committedDefaults = [("KEY", "committed")]
          localOverrides = [("KEY", "local")]
          environmentOverrides = [("KEY", "environment")]
      CoreConfig.lookupConfigValue "KEY" committedDefaults localOverrides environmentOverrides
        `shouldBe` Just "environment"

    it "uses the last declaration within each layer" $ do
      let committedDefaults = [("KEY", "first"), ("KEY", "second")]
      CoreConfig.lookupConfigValue "KEY" committedDefaults [] []
        `shouldBe` Just "second"
      CoreConfig.lookupConfigValue "MISSING" committedDefaults [] []
        `shouldBe` Nothing

  describe "parsePositiveInt" $ do
    it "accepts positive integers" $
      CoreConfig.parsePositiveInt "PORT" "5001"
        `shouldBe` Right 5001

    it "rejects zero, negatives, and non-numeric values" $ do
      CoreConfig.parsePositiveInt "PORT" "0"
        `shouldBe` Left (CoreConfig.InvalidConfigValue "PORT" "0")
      CoreConfig.parsePositiveInt "PORT" "-1"
        `shouldBe` Left (CoreConfig.InvalidConfigValue "PORT" "-1")
      CoreConfig.parsePositiveInt "PORT" "abc"
        `shouldBe` Left (CoreConfig.InvalidConfigValue "PORT" "abc")

  describe "parseNonNegativeInt" $ do
    it "accepts zero and positive integers" $ do
      CoreConfig.parseNonNegativeInt "CACHE" "0"
        `shouldBe` Right 0
      CoreConfig.parseNonNegativeInt "CACHE" "60"
        `shouldBe` Right 60

    it "rejects negatives and non-numeric values" $ do
      CoreConfig.parseNonNegativeInt "CACHE" "-1"
        `shouldBe` Left (CoreConfig.InvalidConfigValue "CACHE" "-1")
      CoreConfig.parseNonNegativeInt "CACHE" "nope"
        `shouldBe` Left (CoreConfig.InvalidConfigValue "CACHE" "nope")

  describe "parseDelimitedTexts" $ do
    it "parses comma-delimited values" $
      CoreConfig.parseDelimitedTexts "EMAILS" "ops@example.com, alerts@example.com"
        `shouldBe` Right ["ops@example.com", "alerts@example.com"]

    it "rejects empty results" $
      CoreConfig.parseDelimitedTexts "EMAILS" " , "
        `shouldBe` Left (CoreConfig.InvalidConfigValue "EMAILS" " , ")

  describe "parseDelimitedTextsUnsafe" $ do
    it "trims whitespace and removes empty entries" $
      CoreConfig.parseDelimitedTextsUnsafe ";" " first ; ; second ; "
        `shouldBe` ["first", "second"]

  describe "parseHeadersUnsafe" $ do
    it "parses valid header pairs and skips malformed entries" $
      CoreConfig.parseHeadersUnsafe "authorization=Bearer token; broken; x-request-id = 123 "
        `shouldBe` [ ("authorization", "Bearer token"),
                     ("x-request-id", "123")
                   ]

  describe "declaredIndices" $ do
    it "extracts sorted unique indices while ignoring malformed keys" $ do
      let entries =
            [ ("LISTENER_2_PORT", "5002"),
              ("LISTENER_1_HOST", "127.0.0.1"),
              ("LISTENER_2_HOST", "127.0.0.2"),
              ("LISTENER_BAD_HOST", "ignored"),
              ("LISTENER_1", "ignored")
            ]
      CoreConfig.declaredIndices "LISTENER_" entries
        `shouldBe` [1, 2]
      CoreConfig.declaredIndices "SERVER_" entries
        `shouldBe` []

  describe "indexedConfigKey" $ do
    it "builds indexed configuration keys predictably" $
      CoreConfig.indexedConfigKey "LISTENER" 3 "PORT"
        `shouldBe` "LISTENER_3_PORT"

  describe "config error rendering" $ do
    it "renders parse errors predictably" $ do
      let missingPort = CoreConfig.MissingConfigValue "PORT"
          invalidPort = CoreConfig.InvalidConfigValue "PORT" "abc"
          brokenLine = CoreConfig.InvalidConfigOverridesLine 2 "BROKEN_LINE"
      show (CoreConfig.MissingConfigValue "PORT")
        `shouldBe` "MissingConfigValue \"PORT\""
      show (CoreConfig.InvalidConfigValue "PORT" "abc")
        `shouldBe` "InvalidConfigValue \"PORT\" \"abc\""
      showsPrec 11 missingPort ""
        `shouldBe` "(MissingConfigValue \"PORT\")"
      showsPrec 11 invalidPort ""
        `shouldBe` "(InvalidConfigValue \"PORT\" \"abc\")"
      show [missingPort]
        `shouldBe` "[MissingConfigValue \"PORT\"]"
      show [brokenLine]
        `shouldBe` "[InvalidConfigOverridesLine 2 \"BROKEN_LINE\"]"
      missingPort `shouldBe` missingPort
      missingPort `shouldNotBe` invalidPort
      brokenLine `shouldBe` brokenLine
      brokenLine `shouldNotBe` CoreConfig.InvalidConfigOverridesLine 3 "OTHER_LINE"
