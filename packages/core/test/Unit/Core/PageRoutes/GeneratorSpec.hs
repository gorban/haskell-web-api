{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Core.PageRoutes.Generator (GenerationError (..), GenerationOutcome (..), GeneratorConfig (..), PageSpec (..), defaultGeneratorConfig, discoverPages, generatePageModules, pageSpecFromRelativePath, renderDispatcherModule, renderManifest, renderRouteModule, validatePageSpecs)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

spec =
  describe "Core.PageRoutes.Generator" $ do
    it "derives nested constructors, modules, paths, and stable content hashes" $ do
      let loginSource = "pageDefinition :: RouteDefinition route context\npageDefinition = login\n"
          changedSource = loginSource <> "login = ()\n"
          loginPage = pageSpecFromRelativePath "App.Pages." "Account/Login.hs" loginSource
          changedLoginPage = pageSpecFromRelativePath "App.Pages." "Account/Login.hs" changedSource
      expectAll
        ( ((pageSourcePath <$> loginPage) `shouldBe` Right "Account/Login.hs")
            :| [ (pageConstructor <$> loginPage) `shouldBe` Right "AccountLoginPage",
                 (pageModuleName <$> loginPage) `shouldBe` Right "App.Pages.Account.Login",
                 (pageUrlPath <$> loginPage) `shouldBe` Right "/account/login",
                 (pageSourceHash <$> loginPage) `shouldNotBe` (pageSourceHash <$> changedLoginPage),
                 pageSpecFromRelativePath "App.Pages." "Home.hs" "pageDefinition = home"
                   `shouldSatisfy` hasConstructor "HomePage",
                 pageSpecFromRelativePath "App.Pages." "NotFound.hs" "pageDefinition = missing"
                   `shouldSatisfy` hasPage "PageNotFound" "/404"
               ]
        )

    it "keeps generator model values comparable and inspectable" $ do
      let config = defaultGeneratorConfig "pages" "generated"
          sameConfig = defaultGeneratorConfig "pages" "generated"
          otherConfig = defaultGeneratorConfig "other-pages" "generated"
          pageSpec = PageSpec "Home.hs" "HomePage" "App.Pages.Home" "/" "home-hash"
          samePageSpec = PageSpec "Home.hs" "HomePage" "App.Pages.Home" "/" "home-hash"
          otherPageSpec = PageSpec "Second.hs" "SecondPage" "App.Pages.Second" "/second" "second-hash"
          missingError = PagesDirectoryMissing "missing"
          sameMissingError = PagesDirectoryMissing "missing"
          noPagesError = NoPagesDiscovered "empty"
          errors =
            [ missingError,
              noPagesError,
              InvalidPageModulePrefix "WebApi.Pages",
              InvalidPagePath "bad.txt",
              MissingPageDefinition "Missing.hs",
              ConstructorCollision "SamePage" ["A.hs", "B.hs"],
              PathCollision "/same" ["A.hs", "B.hs"]
            ]
          outcomes = [Generated ["Route.hs"], Unchanged ["Route.hs"]]
      exerciseModel config sameConfig otherConfig
      exerciseModel pageSpec samePageSpec otherPageSpec
      exerciseModel missingError sameMissingError noPagesError
      exerciseModel (Generated ["Route.hs"]) (Generated ["Route.hs"]) (Unchanged ["Route.hs"])
      expectAll
        ( (show config `shouldContain` "App.Pages.Route.Generated")
            :| [ show pageSpec `shouldContain` "HomePage",
                 map show errors `shouldSatisfy` (not . any null),
                 map show outcomes
                   `shouldBe` ["Generated [\"Route.hs\"]", "Unchanged [\"Route.hs\"]"]
               ]
        )

    it "rejects malformed page namespaces, unsupported module paths, and missing conventional definitions" $
      expectAll
        ( ( pageSpecFromRelativePath "WebApi.Pages" "Login.hs" "pageDefinition = login"
              `shouldBe` Left (InvalidPageModulePrefix "WebApi.Pages")
          )
            :| [ pageSpecFromRelativePath ".WebApi.Pages." "Login.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPageModulePrefix ".WebApi.Pages."),
                 pageSpecFromRelativePath "WebApi..Pages." "Login.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPageModulePrefix "WebApi..Pages."),
                 pageSpecFromRelativePath "webApi.Pages." "Login.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPageModulePrefix "webApi.Pages."),
                 pageSpecFromRelativePath "App.Pages." "account/Login.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "account/Login.hs"),
                 pageSpecFromRelativePath "App.Pages." "Account/Login.txt" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "Account/Login.txt"),
                 pageSpecFromRelativePath "App.Pages." "Account/Log-in.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "Account/Log-in.hs"),
                 pageSpecFromRelativePath "App.Pages." "Account/.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "Account/.hs"),
                 pageSpecFromRelativePath "App.Pages." "Login.hs" "-- pageDefinition is intentionally absent"
                   `shouldBe` Left (MissingPageDefinition "Login.hs")
               ]
        )

    it "reports every source participating in constructor and URL collisions" $ do
      let flatPage = PageSpec "FooBar.hs" "FooBarPage" "App.Pages.FooBar" "/foo-bar" "flat"
          nestedPage = PageSpec "Foo/Bar.hs" "FooBarPage" "App.Pages.Foo.Bar" "/foo/bar" "nested"
          firstPathPage = flatPage {pageUrlPath = "/same"}
          secondPathPage =
            nestedPage
              { pageConstructor = "OtherPage",
                pageUrlPath = "/same"
              }
      expectAll
        ( ( validatePageSpecs [nestedPage, flatPage]
              `shouldBe` Left (ConstructorCollision "FooBarPage" ["Foo/Bar.hs", "FooBar.hs"])
          )
            :| [ validatePageSpecs [secondPathPage, firstPathPage]
                   `shouldBe` Left (PathCollision "/same" ["Foo/Bar.hs", "FooBar.hs"])
               ]
        )

    it "renders the closed route, exhaustive dispatcher, and input manifest deterministically" $ do
      let pageSpecs = samplePageSpecs
          config = defaultGeneratorConfig "pages" "generated"
          routeSource = renderRouteModule config pageSpecs
          dispatcherSource = renderDispatcherModule config pageSpecs
          -- An application whose page handler renders from running
          -- configuration declares the context type, and the dispatcher
          -- threads it to each page's own definition.
          contextConfig =
            config
              { pageDefinitionContextTypeName = Just "AppContext",
                pageDefinitionContextModuleName = Just "App.Context"
              }
          contextDispatcherSource = renderDispatcherModule contextConfig pageSpecs
      expectAll
        ( (routeSource `shouldContain` "data PageRoute\n  = HomePage")
            :| [ routeSource `shouldContain` "PageNotFound -> \"/404\"",
                 routeSource `shouldContain` "\"/second\" -> Just SecondPage",
                 dispatcherSource `shouldContain` "HomePage -> App.Pages.Home.pageDefinition",
                 dispatcherSource `shouldContain` "PageNotFound -> App.Pages.NotFound.pageDefinition",
                 dispatcherSource `shouldContain` "pageRouteDefinition :: PageRoute -> RouteDefinition",
                 contextDispatcherSource
                   `shouldContain` "pageRouteDefinition :: AppContext -> PageRoute -> RouteDefinition",
                 contextDispatcherSource `shouldContain` "import App.Context (AppContext)",
                 dispatcherSource `shouldNotContain` "import App.Context (AppContext)",
                 contextDispatcherSource `shouldContain` "pageRouteDefinition context route =",
                 contextDispatcherSource
                   `shouldContain` "HomePage -> App.Pages.Home.pageDefinition context",
                 renderManifest pageSpecs
                   `shouldBe` "Home.hs\thome-hash\nNotFound.hs\tnot-found-hash\nSecond.hs\tsecond-hash\n",
                 renderRouteModule config [] `shouldContain` "  = NoPagesGenerated",
                 validatePageSpecs [] `shouldBe` Right []
               ]
        )

    it "discovers additions and removals recursively while ignoring non-Haskell files" $
      withSystemTempDirectory "harch-page-discovery" $ \temporaryDirectory -> do
        let pagesDirectory = temporaryDirectory </> "pages"
        writePage pagesDirectory "Home.hs" "pageDefinition = home"
        writePage pagesDirectory "Account/Login.hs" "pageDefinition = login"
        writePage pagesDirectory "notes.txt" "not a module"
        firstDiscovery <- discoverPages "App.Pages." pagesDirectory
        firstDiscovery `shouldSatisfy` hasConstructors ["AccountLoginPage", "HomePage"]
        writePage pagesDirectory "Second.hs" "pageDefinition = second"
        secondDiscovery <- discoverPages "App.Pages." pagesDirectory
        secondDiscovery
          `shouldSatisfy` hasConstructors ["AccountLoginPage", "HomePage", "SecondPage"]
        removeFile (pagesDirectory </> "Account/Login.hs")
        thirdDiscovery <- discoverPages "App.Pages." pagesDirectory
        thirdDiscovery `shouldSatisfy` hasConstructors ["HomePage", "SecondPage"]
        -- The module namespace is the application's, so discovery takes it as an
        -- argument rather than assuming App.Pages.
        prefixedDiscovery <- discoverPages "WebApi.Pages." pagesDirectory
        prefixedDiscovery
          `shouldSatisfy` hasModules ["WebApi.Pages.Home", "WebApi.Pages.Second"]
        invalidPrefixDiscovery <- discoverPages "webApi.Pages." pagesDirectory
        invalidPrefixDiscovery `shouldBe` Left (InvalidPageModulePrefix "webApi.Pages.")

    it "reports missing and empty page roots explicitly" $
      withSystemTempDirectory "harch-empty-pages" $ \temporaryDirectory -> do
        let missingDirectory = temporaryDirectory </> "missing"
            emptyDirectory = temporaryDirectory </> "empty"
        createDirectoryIfMissing True emptyDirectory
        expectAll
          ( ( discoverPages "App.Pages." missingDirectory
                `shouldReturn` Left (PagesDirectoryMissing missingDirectory)
            )
              :| [ discoverPages "App.Pages." emptyDirectory
                     `shouldReturn` Left (NoPagesDiscovered emptyDirectory)
                 ]
          )

    it "propagates discovery failures through the generation entry point" $
      withSystemTempDirectory "harch-missing-generation" $ \temporaryDirectory -> do
        let missingDirectory = temporaryDirectory </> "missing"
            config = defaultGeneratorConfig missingDirectory (temporaryDirectory </> "generated")
        generatePageModules config
          `shouldReturn` Left (PagesDirectoryMissing missingDirectory)

    it "writes changed outputs once and preserves no-op incremental builds" $
      withSystemTempDirectory "harch-page-generation" $ \temporaryDirectory -> do
        let pagesDirectory = temporaryDirectory </> "pages"
            generatedDirectory = temporaryDirectory </> "generated"
            config =
              (defaultGeneratorConfig pagesDirectory generatedDirectory)
                { applicationRouteModuleName = "Example.Route",
                  applicationRouteTypeName = "ExampleRoute",
                  requestContextTypeName = "ExampleContext"
                }
        writePage pagesDirectory "Home.hs" "pageDefinition = home"
        firstResult <- generatePageModules config
        secondResult <- generatePageModules config
        writePage pagesDirectory "Second.hs" "pageDefinition = second"
        thirdResult <- generatePageModules config
        let expectedOutputs =
              [ generatedDirectory </> "App/Pages/Route/Generated.hs",
                generatedDirectory </> "App/Pages/Generated.hs",
                generatedDirectory </> "harch-page-routes.manifest"
              ]
        outputPresence <- traverse doesFileExist expectedOutputs
        dispatcherSource <- TextIO.readFile (generatedDirectory </> "App/Pages/Generated.hs")
        expectAll
          ( (firstResult `shouldBe` Right (Generated expectedOutputs))
              :| [ secondResult `shouldBe` Right (Unchanged expectedOutputs),
                   thirdResult `shouldBe` Right (Generated expectedOutputs),
                   outputPresence `shouldBe` [True, True, True],
                   dispatcherSource
                     `shouldSatisfy` Text.isInfixOf "RouteDefinition ExampleRoute ExampleContext ()",
                   dispatcherSource
                     `shouldSatisfy` Text.isInfixOf "SecondPage -> App.Pages.Second.pageDefinition"
                 ]
          )

    it "writes dispatchers that import pages below another application namespace" $
      withSystemTempDirectory "harch-prefixed-page-generation" $ \temporaryDirectory -> do
        let pagesDirectory = temporaryDirectory </> "pages"
            generatedDirectory = temporaryDirectory </> "generated"
            config =
              (defaultGeneratorConfig pagesDirectory generatedDirectory)
                { pageModulePrefix = "WebApi.Pages.",
                  routeModuleName = "WebApi.Pages.Route.Generated",
                  dispatcherModuleName = "WebApi.Pages.Generated",
                  applicationRouteModuleName = "WebApi.Route",
                  applicationRouteTypeName = "AppRoute",
                  requestContextTypeName = "AppRequestContext"
                }
        writePage pagesDirectory "Home.hs" "pageDefinition = home"
        generated <- generatePageModules config
        dispatcherSource <- TextIO.readFile (generatedDirectory </> "WebApi/Pages/Generated.hs")
        expectAll
          ( ( generated
                `shouldBe` Right
                  ( Generated
                      [ generatedDirectory </> "WebApi/Pages/Route/Generated.hs",
                        generatedDirectory </> "WebApi/Pages/Generated.hs",
                        generatedDirectory </> "harch-page-routes.manifest"
                      ]
                  )
            )
              :| [ dispatcherSource
                     `shouldSatisfy` Text.isInfixOf "HomePage -> WebApi.Pages.Home.pageDefinition",
                   dispatcherSource
                     `shouldSatisfy` Text.isInfixOf "RouteDefinition AppRoute AppRequestContext ()"
                 ]
          )

samplePageSpecs :: [PageSpec]
samplePageSpecs =
  [ PageSpec "Home.hs" "HomePage" "App.Pages.Home" "/" "home-hash",
    PageSpec "NotFound.hs" "PageNotFound" "App.Pages.NotFound" "/404" "not-found-hash",
    PageSpec "Second.hs" "SecondPage" "App.Pages.Second" "/second" "second-hash"
  ]

hasConstructor :: String -> Either GenerationError PageSpec -> Bool
hasConstructor expectedConstructor pageResult =
  case pageResult of
    Right pageSpec -> pageConstructor pageSpec == expectedConstructor
    Left _ -> False

hasPage :: String -> Text.Text -> Either GenerationError PageSpec -> Bool
hasPage expectedConstructor expectedPath pageResult =
  case pageResult of
    Right pageSpec ->
      pageConstructor pageSpec == expectedConstructor && pageUrlPath pageSpec == expectedPath
    Left _ -> False

exerciseModel :: (Eq value, Show value) => value -> value -> value -> Expectation
exerciseModel value sameValue otherValue = do
  (value == sameValue) `shouldBe` True
  (value /= otherValue) `shouldBe` True
  show value `shouldSatisfy` not . null
  showsPrec 11 value "" `shouldSatisfy` not . null
  showList [value, otherValue] "" `shouldSatisfy` not . null

hasConstructors :: [String] -> Either GenerationError [PageSpec] -> Bool
hasConstructors expectedConstructors pageResult =
  case pageResult of
    Right pageSpecs -> map pageConstructor pageSpecs == expectedConstructors
    Left _ -> False

hasModules :: [String] -> Either GenerationError [PageSpec] -> Bool
hasModules expectedModules pageResult =
  case pageResult of
    Right pageSpecs -> map pageModuleName pageSpecs == expectedModules
    Left _ -> False

writePage :: FilePath -> FilePath -> String -> IO ()
writePage pagesDirectory relativePath source = do
  let outputPath = pagesDirectory </> relativePath
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeFile outputPath source
