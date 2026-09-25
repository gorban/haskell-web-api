{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

-- | Cabal's Hooks build type owns the package lifecycle rather than a Custom
-- @Setup.hs@ executable.  Declared @build-tool-depends@ provide the normal
-- test-before-tool ordering.  This hook retains only the separate, opt-in
-- database prerequisite workflow: record an autostart during configuration,
-- then migrate after Cabal has built the declared database executable.
module SetupHooks (setupHooks) where

import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Core.PageRoutes.Generator (GeneratorConfig (applicationRouteModuleName, applicationRouteTypeName, authorizationTypeName, dispatcherModuleName, pageDefinitionContextModuleName, pageDefinitionContextTypeName, pageModulePrefix, requestContextTypeName, routeModuleName), defaultGeneratorConfig, generatePageModules)
import Core.Setup.PrerequisiteReport
  ( DatabasePrerequisiteStatus (DatabasePrerequisiteAutostarted),
    SetupPrerequisiteReport (databasePrerequisiteStatus),
    reportSetupPrerequisitesAndReturn,
  )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.LocalBuildInfo (buildDir)
import Distribution.Simple.SetupHooks (BuildHooks (postBuildComponentHook, preBuildComponentRules), Component (CExe), ComponentName (CLibName), ConfigureHooks (preConfPackageHook), Dependency (FileDependency), Dict (..), Executable (exeName), LibraryName (LMainLibName), LocalBuildInfo, Location (..), PostBuildComponentInputs (PostBuildComponentInputs, localBuildInfo, targetInfo), PreBuildComponentInputs (..), PreBuildComponentRules, PreConfPackageInputs, PreConfPackageOutputs, RulesM, SetupHooks, TargetInfo (targetCLBI, targetComponent), addRuleMonitors, autogenComponentModulesDir, buildHooks, componentName, configureHooks, mkCommand, monitorDirectory, noBuildHooks, noConfigureHooks, noPreConfPackageOutputs, noSetupHooks, registerRule_, rules, staticRule)
import Distribution.Types.LocalBuildInfo (hostPlatform)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath, makeRelativePathEx, makeSymbolicPath)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute, removeFile)
import System.FilePath (takeExtension, (</>))
import System.Process (callProcess)

-- | The package's one Hooks value.  Configuration does not migrate a database
-- because the executable is not built yet; the post-build hook runs it only
-- after the component Cabal itself named as the migration tool is available.
setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
          { preConfPackageHook = Just recordDatabaseAutostart
          },
      buildHooks =
        noBuildHooks
          { postBuildComponentHook = Just runDatabaseSetupIfNeeded,
            preBuildComponentRules = Just pageRouteRules
          }
    }

recordDatabaseAutostart :: PreConfPackageInputs -> IO PreConfPackageOutputs
recordDatabaseAutostart inputs = do
  reportedPrerequisites <- reportSetupPrerequisitesAndReturn
  if databaseWasAutostarted reportedPrerequisites
    then writeFile databaseSetupStatePath "autostarted\n"
    else clearDatabaseSetupState
  pure (noPreConfPackageOutputs inputs)

databaseWasAutostarted :: Either loadError SetupPrerequisiteReport -> Bool
databaseWasAutostarted reportedPrerequisites =
  case reportedPrerequisites of
    Right prerequisiteReport ->
      case databasePrerequisiteStatus prerequisiteReport of
        DatabasePrerequisiteAutostarted _ _ -> True
        _ -> False
    Left _ -> False

runDatabaseSetupIfNeeded :: PostBuildComponentInputs -> IO ()
runDatabaseSetupIfNeeded (PostBuildComponentInputs {localBuildInfo = setupBuildInfo, targetInfo = setupTarget}) =
  when (isDatabaseSetupExecutable setupTarget) $ do
    setupStateExists <- doesFileExist databaseSetupStatePath
    when setupStateExists $ do
      putStrLn "Setup: Running database migrations and seed data via haskell-web-api-db."
      callProcess (builtExecutablePath setupBuildInfo "haskell-web-api-db") ["migrate-and-seed"]
      clearDatabaseSetupState

isDatabaseSetupExecutable :: TargetInfo -> Bool
isDatabaseSetupExecutable target =
  case targetComponent target of
    CExe executable -> exeName executable == mkUnqualComponentName "haskell-web-api-db"
    _ -> False

databaseSetupStatePath :: FilePath
databaseSetupStatePath = ".setup-postgres-autostarted"

clearDatabaseSetupState :: IO ()
clearDatabaseSetupState = do
  setupStateExists <- doesFileExist databaseSetupStatePath
  when setupStateExists (removeFile databaseSetupStatePath)

builtExecutablePath :: LocalBuildInfo -> FilePath -> FilePath
builtExecutablePath localBuildInfo executableName =
  executableBasePath <> exeExtension (hostPlatform localBuildInfo)
  where
    executableBasePath =
      getSymbolicPath (buildDir localBuildInfo)
        </> executableName
        </> executableName

-- | Discover each 'WebApi.Pages' module and generate the route sum and
-- definition registry before the library builds, mirroring the proven
-- examples/two-pages wiring. The file name implies the route
-- ('WebApi.Pages.Showcase' -> 'ShowcasePage' -> \"/showcase\").
pageRouteRules :: PreBuildComponentRules
pageRouteRules =
  rules (static ()) routeRulesForInputs

routeRulesForInputs :: PreBuildComponentInputs -> RulesM ()
routeRulesForInputs inputs@(PreBuildComponentInputs {targetInfo = rulesTarget}) =
  case componentName (targetComponent rulesTarget) of
    CLibName LMainLibName -> do
      registerPageRouteRule inputs
    _ -> pure ()

registerPageRouteRule :: PreBuildComponentInputs -> RulesM ()
registerPageRouteRule
  PreBuildComponentInputs
    { localBuildInfo = buildInfo,
      targetInfo = target
    } = do
    let pagesDirectory = "src/WebApi/Pages"
        generatedDirectory =
          autogenComponentModulesDir
            buildInfo
            (targetCLBI target)
    (sourceDirectories, sourceFiles) <- liftIO (discoverPageInputs pagesDirectory)
    monitoredDirectories <- liftIO (traverse makeAbsolute sourceDirectories)
    addRuleMonitors (map monitorDirectory monitoredDirectories)
    registerRule_ "harch-page-routes" $
      staticRule
        ( mkCommand
            (static Dict)
            (static runPageGeneration)
            (pagesDirectory, getSymbolicPath generatedDirectory)
        )
        [ FileDependency
            (Location (makeSymbolicPath ".") (makeRelativePathEx sourceFile))
        | sourceFile <- sourceFiles
        ]
        ( Location generatedDirectory (makeRelativePathEx "WebApi/Pages/Route/Generated.hs")
            :| [ Location generatedDirectory (makeRelativePathEx "WebApi/Pages/Generated.hs"),
                 Location generatedDirectory (makeRelativePathEx "harch-page-routes.manifest")
               ]
        )

runPageGeneration :: (FilePath, FilePath) -> IO ()
runPageGeneration (pagesDirectory, generatedDirectory) = do
  generationResult <-
    generatePageModules
      ( (defaultGeneratorConfig pagesDirectory generatedDirectory)
          { pageModulePrefix = "WebApi.Pages.",
            routeModuleName = "WebApi.Pages.Route.Generated",
            dispatcherModuleName = "WebApi.Pages.Generated",
            applicationRouteModuleName = "WebApi.Route",
            applicationRouteTypeName = "AppRoute",
            requestContextTypeName = "AppRequestContext",
            authorizationTypeName = "AppAuthorization",
            pageDefinitionContextTypeName = Just "AppConfig",
            pageDefinitionContextModuleName = Just "WebApi.Config"
          }
      )
  either (ioError . userError . show) (const (pure ())) generationResult

discoverPageInputs :: FilePath -> IO ([FilePath], [FilePath])
discoverPageInputs directory = do
  entries <- listDirectory directory
  let paths = map (directory </>) entries
  directories <- filterM doesDirectoryExist paths
  files <- filterM doesFileExist paths
  nestedInputs <- traverse discoverPageInputs directories
  pure
    ( directory : concatMap fst nestedInputs,
      filter ((== ".hs") . takeExtension) files <> concatMap snd nestedInputs
    )
