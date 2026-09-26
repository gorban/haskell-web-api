{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import HarchWeb (AssetPath (..), Page (..), RuntimeDescriptor (..), StaticAssetRoot (..), renderHtml, safeUrlText, stylesheet)
import HarchWeb.OpenApi.Swagger (SwaggerUiProps (..), defaultSwaggerUiProps, requiredSwaggerSpecUrlOrDie, swaggerUiAssetsRoot, swaggerUiFallback, swaggerUiPage, swaggerUiPageEnhancement, swaggerUiStylesheet)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

spec = describe "HarchWeb.OpenApi.Swagger" $ do
  it "defaults to the task file's documented endpoints and asset locations" $ do
    let props = defaultSwaggerUiProps ("route-value" :: String) ("context-value" :: String)
    swaggerUiTitle props `shouldBe` "Documentation"
    safeUrlText (swaggerUiSpecUrl props) `shouldBe` "/docs/openapi.json"
    swaggerUiBundleUrl props `shouldBe` "/docs/assets/swagger-ui-bundle.js"
    swaggerUiStylesheetUrl props `shouldBe` "/docs/assets/swagger-ui.css"
    swaggerUiModuleUrl props `shouldBe` "/docs/assets/swagger-enhancement.js"
    swaggerUiTokenEndpoint props `shouldBe` "/api/oauth/token"
    swaggerUiExampleClientId props `shouldBe` Nothing
    swaggerUiExampleClientSecret props `shouldBe` Nothing

  it "renders a complete script-free SSR page with an enhancement mount" $ do
    let props = defaultSwaggerUiProps ("route-value" :: String) ("context-value" :: String)
        rendered = Text.unpack (renderHtml (pageBody (swaggerUiPage props)))
    pageTitle (swaggerUiPage props) `shouldBe` "Documentation"
    pageRoute (swaggerUiPage props) `shouldBe` ("route-value" :: String)
    pageContext (swaggerUiPage props) `shouldBe` ("context-value" :: String)
    pageBootstrapHooks (swaggerUiPage props) `shouldBe` []
    pageStylesheets (swaggerUiPage props) `shouldBe` []
    expectAll
      ( (rendered `shouldContain` "<h1>Documentation</h1>")
          :| [ rendered `shouldContain` "data-swagger-fallback=\"true\"",
               rendered `shouldContain` "data-swagger-ui=\"true\"",
               rendered `shouldContain` "data-swagger-spec-url=\"/docs/openapi.json\"",
               rendered `shouldContain` "data-swagger-bundle-url=\"/docs/assets/swagger-ui-bundle.js\"",
               rendered `shouldContain` "data-swagger-token-endpoint=\"/api/oauth/token\"",
               rendered `shouldContain` "Open the OpenAPI document",
               rendered `shouldContain` "href=\"/docs/openapi.json\""
             ]
      )
    -- The scripts-disabled acceptance case: the server-rendered surface must
    -- carry no script elements at all, so the document is complete and
    -- honest before (or without) any enhancement.
    rendered `shouldNotContain` "<script"

  it "renders authored example credentials into the mount attributes" $ do
    let props =
          (defaultSwaggerUiProps ("route-value" :: String) ("context-value" :: String))
            { swaggerUiExampleClientId = Just "demo-client",
              swaggerUiExampleClientSecret = Just "demo-secret"
            }
        rendered = Text.unpack (renderHtml (pageBody (swaggerUiPage props)))
    expectAll
      ( (rendered `shouldContain` "data-swagger-example-client-id=\"demo-client\"")
          :| [ rendered `shouldContain` "data-swagger-example-client-secret=\"demo-secret\""
             ]
      )

  it "keeps the replaceable fallback swappable without touching the mount" $ do
    let customSpecUrl =
          requiredSwaggerSpecUrlOrDie "/custom/openapi.json"
        props =
          (defaultSwaggerUiProps ("route-value" :: String) ("context-value" :: String))
            { swaggerUiFallbackBody = swaggerUiFallback customSpecUrl
            }
        rendered = Text.unpack (renderHtml (pageBody (swaggerUiPage props)))
    expectAll
      ( (rendered `shouldContain` "href=\"/custom/openapi.json\"")
          :| [ rendered `shouldContain` "data-swagger-ui=\"true\"",
               rendered `shouldContain` "data-swagger-spec-url=\"/docs/openapi.json\""
             ]
      )

  it "declares the page-scoped enhancement and stylesheet against the props URLs" $ do
    let props = defaultSwaggerUiProps ("route-value" :: String) ("context-value" :: String)
    swaggerUiPageEnhancement props `shouldBe` PageEnhancementModule "harch-swagger-ui" "/docs/assets/swagger-enhancement.js"
    swaggerUiStylesheet props `shouldBe` stylesheet (AssetPath "/docs/assets/swagger-ui.css")

  it "serves the pinned distribution from a real assets root" $ do
    root <- swaggerUiAssetsRoot "/docs/assets"
    staticUrlPrefix root `shouldBe` "/docs/assets"
    let directory = staticDirectory root
    exists <- doesDirectoryExist directory
    exists `shouldBe` True
    mapM_
      ( \fileName -> do
          fileExists <- doesFileExist (directory </> fileName)
          fileExists `shouldBe` True
      )
      [ "swagger-ui-bundle.js",
        "swagger-ui.css",
        "swagger-enhancement.js",
        "LICENSE.apache-2.0",
        "NOTICE"
      ]

  it "fails immediately when a program-owned specification URL literal is invalid" $ do
    requiredSwaggerSpecUrlOrDie "/docs/openapi.json" `seq` pure ()
    evaluate (requiredSwaggerSpecUrlOrDie "javascript:alert(1)" `seq` ())
      `shouldThrow` \case
        ErrorCall message ->
          message == "HarchWeb.Markup: HarchWeb.OpenApi.Swagger: invalid specification URL: javascript:alert(1)"

  it "never reads cookies or browser storage in the behavior module" $ do
    root <- swaggerUiAssetsRoot "/docs/assets"
    source <- readFile (staticDirectory root </> "swagger-enhancement.js")
    expectAll
      ( (source `shouldNotContain` "document.cookie")
          :| [ source `shouldNotContain` "localStorage",
               source `shouldNotContain` "sessionStorage",
               source `shouldContain` "memory-only"
             ]
      )
