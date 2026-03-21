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
            [ Text.pack "# override file",
              Text.pack " APP_TITLE_PREFIX = custom-app ",
              Text.empty,
              Text.pack "LISTENER_0_PORT=6001"
            ]
        )
        `shouldBe` Right
          [ (Text.pack "APP_TITLE_PREFIX", Text.pack "custom-app"),
            (Text.pack "LISTENER_0_PORT", Text.pack "6001")
          ]

    it "rejects malformed override lines with the original line content" $ do
      CoreConfig.parseConfigOverridesFile
        ( Text.unlines
            [ Text.pack "APP_TITLE_PREFIX=custom-app",
              Text.pack "BROKEN_LINE"
            ]
        )
        `shouldBe` Left (CoreConfig.InvalidConfigOverridesLine 2 (Text.pack "BROKEN_LINE"))
      CoreConfig.parseConfigOverridesFile (Text.pack "   =value")
        `shouldBe` Left (CoreConfig.InvalidConfigOverridesLine 1 (Text.pack "   =value"))
      show (CoreConfig.InvalidConfigOverridesLine 2 (Text.pack "BROKEN_LINE"))
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
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "loaded-from-file"),
              (Text.pack "LISTENER_0_PORT", Text.pack "5100")
            ]

  describe "lookupConfigValue" $ do
    it "prefers environment overrides over local overrides over committed defaults" $ do
      let committedDefaults = [(Text.pack "KEY", Text.pack "committed")]
          localOverrides = [(Text.pack "KEY", Text.pack "local")]
          environmentOverrides = [(Text.pack "KEY", Text.pack "environment")]
      CoreConfig.lookupConfigValue (Text.pack "KEY") committedDefaults localOverrides environmentOverrides
        `shouldBe` Just (Text.pack "environment")

    it "uses the last declaration within each layer" $ do
      let committedDefaults = [(Text.pack "KEY", Text.pack "first"), (Text.pack "KEY", Text.pack "second")]
      CoreConfig.lookupConfigValue (Text.pack "KEY") committedDefaults [] []
        `shouldBe` Just (Text.pack "second")
      CoreConfig.lookupConfigValue (Text.pack "MISSING") committedDefaults [] []
        `shouldBe` Nothing

  describe "parsePositiveInt" $ do
    it "accepts positive integers" $
      CoreConfig.parsePositiveInt (Text.pack "PORT") (Text.pack "5001")
        `shouldBe` Right 5001

    it "rejects zero, negatives, and non-numeric values" $ do
      CoreConfig.parsePositiveInt (Text.pack "PORT") (Text.pack "0")
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "PORT") (Text.pack "0"))
      CoreConfig.parsePositiveInt (Text.pack "PORT") (Text.pack "-1")
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "PORT") (Text.pack "-1"))
      CoreConfig.parsePositiveInt (Text.pack "PORT") (Text.pack "abc")
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "PORT") (Text.pack "abc"))

  describe "parseNonNegativeInt" $ do
    it "accepts zero and positive integers" $ do
      CoreConfig.parseNonNegativeInt (Text.pack "CACHE") (Text.pack "0")
        `shouldBe` Right 0
      CoreConfig.parseNonNegativeInt (Text.pack "CACHE") (Text.pack "60")
        `shouldBe` Right 60

    it "rejects negatives and non-numeric values" $ do
      CoreConfig.parseNonNegativeInt (Text.pack "CACHE") (Text.pack "-1")
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "CACHE") (Text.pack "-1"))
      CoreConfig.parseNonNegativeInt (Text.pack "CACHE") (Text.pack "nope")
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "CACHE") (Text.pack "nope"))

  describe "parseDelimitedTexts" $ do
    it "parses comma-delimited values" $
      CoreConfig.parseDelimitedTexts (Text.pack "EMAILS") (Text.pack "ops@example.com, alerts@example.com")
        `shouldBe` Right [Text.pack "ops@example.com", Text.pack "alerts@example.com"]

    it "rejects empty results" $
      CoreConfig.parseDelimitedTexts (Text.pack "EMAILS") (Text.pack " , ")
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "EMAILS") (Text.pack " , "))

  describe "parseDelimitedTextsUnsafe" $ do
    it "trims whitespace and removes empty entries" $
      CoreConfig.parseDelimitedTextsUnsafe (Text.pack ";") (Text.pack " first ; ; second ; ")
        `shouldBe` [Text.pack "first", Text.pack "second"]

  describe "parseHeadersUnsafe" $ do
    it "parses valid header pairs and skips malformed entries" $
      CoreConfig.parseHeadersUnsafe (Text.pack "authorization=Bearer token; broken; x-request-id = 123 ")
        `shouldBe` [ (Text.pack "authorization", Text.pack "Bearer token"),
                     (Text.pack "x-request-id", Text.pack "123")
                   ]

  describe "declaredIndices" $ do
    it "extracts sorted unique indices while ignoring malformed keys" $ do
      let entries =
            [ (Text.pack "LISTENER_2_PORT", Text.pack "5002"),
              (Text.pack "LISTENER_1_HOST", Text.pack "127.0.0.1"),
              (Text.pack "LISTENER_2_HOST", Text.pack "127.0.0.2"),
              (Text.pack "LISTENER_BAD_HOST", Text.pack "ignored"),
              (Text.pack "LISTENER_1", Text.pack "ignored")
            ]
      CoreConfig.declaredIndices (Text.pack "LISTENER_") entries
        `shouldBe` [1, 2]
      CoreConfig.declaredIndices (Text.pack "SERVER_") entries
        `shouldBe` []

  describe "indexedConfigKey" $ do
    it "builds indexed configuration keys predictably" $
      CoreConfig.indexedConfigKey (Text.pack "LISTENER") 3 (Text.pack "PORT")
        `shouldBe` Text.pack "LISTENER_3_PORT"

  describe "config error rendering" $ do
    it "renders parse errors predictably" $ do
      let missingPort = CoreConfig.MissingConfigValue (Text.pack "PORT")
          invalidPort = CoreConfig.InvalidConfigValue (Text.pack "PORT") (Text.pack "abc")
          brokenLine = CoreConfig.InvalidConfigOverridesLine 2 (Text.pack "BROKEN_LINE")
      show (CoreConfig.MissingConfigValue (Text.pack "PORT"))
        `shouldBe` "MissingConfigValue \"PORT\""
      show (CoreConfig.InvalidConfigValue (Text.pack "PORT") (Text.pack "abc"))
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
      brokenLine `shouldNotBe` CoreConfig.InvalidConfigOverridesLine 3 (Text.pack "OTHER_LINE")
