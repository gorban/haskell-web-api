{-# LANGUAGE OverloadedStrings #-}

module Unit.Core.PageRoutes.GeneratorSpec (spec) where

import Core.PageRoutes.Generator
  ( GenerationError (..),
    GenerationOutcome (..),
    GeneratorConfig (..),
    PageSpec (..),
    defaultGeneratorConfig,
    discoverPages,
    generatePageModules,
    pageSpecFromRelativePath,
    renderDispatcherModule,
    renderManifest,
    renderRouteModule,
    validatePageSpecs,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "Core.PageRoutes.Generator" $ do
    it "derives nested constructors, modules, paths, and stable content hashes" $ do
      let loginSource = "pageDefinition :: RouteDefinition route context\npageDefinition = login\n"
          changedSource = loginSource <> "login = ()\n"
          loginPage = pageSpecFromRelativePath "Account/Login.hs" loginSource
          changedLoginPage = pageSpecFromRelativePath "Account/Login.hs" changedSource
      expectAll
        ( ((pageSourcePath <$> loginPage) `shouldBe` Right "Account/Login.hs")
            :| [ (pageConstructor <$> loginPage) `shouldBe` Right "AccountLoginPage",
                 (pageModuleName <$> loginPage) `shouldBe` Right "App.Pages.Account.Login",
                 (pageUrlPath <$> loginPage) `shouldBe` Right "/account/login",
                 (pageSourceHash <$> loginPage) `shouldNotBe` (pageSourceHash <$> changedLoginPage),
                 pageSpecFromRelativePath "Home.hs" "pageDefinition = home"
                   `shouldSatisfy` hasConstructor "HomePage",
                 pageSpecFromRelativePath "NotFound.hs" "pageDefinition = missing"
                   `shouldSatisfy` hasPage "PageNotFound" "/404"
               ]
        )

    it "keeps generator model values comparable and inspectable" $ do
      let config = defaultGeneratorConfig "pages" "generated"
          otherConfig = defaultGeneratorConfig "other-pages" "generated"
          pageSpec = PageSpec "Home.hs" "HomePage" "App.Pages.Home" "/" "home-hash"
          otherPageSpec = PageSpec "Second.hs" "SecondPage" "App.Pages.Second" "/second" "second-hash"
          missingError = PagesDirectoryMissing "missing"
          noPagesError = NoPagesDiscovered "empty"
          errors =
            [ missingError,
              noPagesError,
              InvalidPagePath "bad.txt",
              MissingPageDefinition "Missing.hs",
              ConstructorCollision "SamePage" ["A.hs", "B.hs"],
              PathCollision "/same" ["A.hs", "B.hs"]
            ]
          outcomes = [Generated ["Route.hs"], Unchanged ["Route.hs"]]
      exerciseModel config otherConfig
      exerciseModel pageSpec otherPageSpec
      exerciseModel missingError noPagesError
      exerciseModel (Generated ["Route.hs"]) (Unchanged ["Route.hs"])
      expectAll
        ( (config `shouldBe` config)
            :| [ show config `shouldContain` "App.Pages.Route.Generated",
                 pageSpec `shouldBe` pageSpec,
                 show pageSpec `shouldContain` "HomePage",
                 errors `shouldBe` errors,
                 map show errors `shouldSatisfy` (not . any null),
                 outcomes `shouldBe` outcomes,
                 map show outcomes
                   `shouldBe` ["Generated [\"Route.hs\"]", "Unchanged [\"Route.hs\"]"]
               ]
        )

    it "rejects unsupported module paths and missing conventional definitions" $ do
      expectAll
        ( ( pageSpecFromRelativePath "account/Login.hs" "pageDefinition = login"
              `shouldBe` Left (InvalidPagePath "account/Login.hs")
          )
            :| [ pageSpecFromRelativePath "Account/Login.txt" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "Account/Login.txt"),
                 pageSpecFromRelativePath "Account/Log-in.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "Account/Log-in.hs"),
                 pageSpecFromRelativePath "Account/.hs" "pageDefinition = login"
                   `shouldBe` Left (InvalidPagePath "Account/.hs"),
                 pageSpecFromRelativePath "Login.hs" "-- pageDefinition is intentionally absent"
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
      expectAll
        ( (routeSource `shouldContain` "data PageRoute\n  = HomePage")
            :| [ routeSource `shouldContain` "PageNotFound -> \"/404\"",
                 routeSource `shouldContain` "\"/second\" -> Just SecondPage",
                 dispatcherSource `shouldContain` "HomePage -> App.Pages.Home.pageDefinition",
                 dispatcherSource `shouldContain` "PageNotFound -> App.Pages.NotFound.pageDefinition",
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
        firstDiscovery <- discoverPages pagesDirectory
        firstDiscovery `shouldSatisfy` hasConstructors ["AccountLoginPage", "HomePage"]
        writePage pagesDirectory "Second.hs" "pageDefinition = second"
        secondDiscovery <- discoverPages pagesDirectory
        secondDiscovery
          `shouldSatisfy` hasConstructors ["AccountLoginPage", "HomePage", "SecondPage"]
        removeFile (pagesDirectory </> "Account/Login.hs")
        thirdDiscovery <- discoverPages pagesDirectory
        thirdDiscovery `shouldSatisfy` hasConstructors ["HomePage", "SecondPage"]

    it "reports missing and empty page roots explicitly" $
      withSystemTempDirectory "harch-empty-pages" $ \temporaryDirectory -> do
        let missingDirectory = temporaryDirectory </> "missing"
            emptyDirectory = temporaryDirectory </> "empty"
        createDirectoryIfMissing True emptyDirectory
        expectAll
          ( ( discoverPages missingDirectory
                `shouldReturn` Left (PagesDirectoryMissing missingDirectory)
            )
              :| [ discoverPages emptyDirectory
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
                     `shouldSatisfy` Text.isInfixOf "RouteDefinition ExampleRoute ExampleContext",
                   dispatcherSource
                     `shouldSatisfy` Text.isInfixOf "SecondPage -> App.Pages.Second.pageDefinition"
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

exerciseModel :: (Eq value, Show value) => value -> value -> Expectation
exerciseModel value otherValue = do
  eqViaDictionary value value `shouldBe` True
  neqViaDictionary value otherValue `shouldBe` True
  showViaDictionary value `shouldSatisfy` not . null
  showsPrecViaDictionary 11 value "" `shouldSatisfy` not . null
  showListViaDictionary [value, otherValue] "" `shouldSatisfy` not . null

eqViaDictionary :: (Eq value) => value -> value -> Bool
eqViaDictionary = (==)
{-# NOINLINE eqViaDictionary #-}

neqViaDictionary :: (Eq value) => value -> value -> Bool
neqViaDictionary = (/=)
{-# NOINLINE neqViaDictionary #-}

showViaDictionary :: (Show value) => value -> String
showViaDictionary = show
{-# NOINLINE showViaDictionary #-}

showsPrecViaDictionary :: (Show value) => Int -> value -> ShowS
showsPrecViaDictionary = showsPrec
{-# NOINLINE showsPrecViaDictionary #-}

showListViaDictionary :: (Show value) => [value] -> ShowS
showListViaDictionary = showList
{-# NOINLINE showListViaDictionary #-}

hasConstructors :: [String] -> Either GenerationError [PageSpec] -> Bool
hasConstructors expectedConstructors pageResult =
  case pageResult of
    Right pageSpecs -> map pageConstructor pageSpecs == expectedConstructors
    Left _ -> False

writePage :: FilePath -> FilePath -> String -> IO ()
writePage pagesDirectory relativePath source = do
  let outputPath = pagesDirectory </> relativePath
  createDirectoryIfMissing True (takeDirectory outputPath)
  writeFile outputPath source
