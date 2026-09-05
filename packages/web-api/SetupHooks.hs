-- | Cabal's Hooks build type owns the package lifecycle rather than a Custom
-- @Setup.hs@ executable.  Declared @build-tool-depends@ provide the normal
-- test-before-tool ordering.  This hook retains only the separate, opt-in
-- database prerequisite workflow: record an autostart during configuration,
-- then migrate after Cabal has built the declared database executable.
module SetupHooks (setupHooks) where

import Control.Monad (when)
import Core.Setup.PrerequisiteReport
  ( DatabasePrerequisiteStatus (DatabasePrerequisiteAutostarted),
    SetupPrerequisiteReport (databasePrerequisiteStatus),
    reportSetupPrerequisitesAndReturn,
  )
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.LocalBuildInfo (buildDir)
import Distribution.Simple.SetupHooks
  ( BuildHooks (postBuildComponentHook),
    Component (CExe),
    ConfigureHooks (preConfPackageHook),
    Executable (exeName),
    LocalBuildInfo,
    PostBuildComponentInputs (localBuildInfo, targetInfo),
    PreConfPackageInputs,
    PreConfPackageOutputs,
    SetupHooks,
    TargetInfo (targetComponent),
    buildHooks,
    configureHooks,
    noBuildHooks,
    noConfigureHooks,
    noPreConfPackageOutputs,
    noSetupHooks,
  )
import Distribution.Types.LocalBuildInfo (hostPlatform)
import Distribution.Types.UnqualComponentName (mkUnqualComponentName)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
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
          { postBuildComponentHook = Just runDatabaseSetupIfNeeded
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
runDatabaseSetupIfNeeded inputs =
  when (isDatabaseSetupExecutable inputs) $ do
    setupStateExists <- doesFileExist databaseSetupStatePath
    when setupStateExists $ do
      putStrLn "Setup: Running database migrations and seed data via haskell-web-api-db."
      callProcess (builtExecutablePath (localBuildInfo inputs) "haskell-web-api-db") ["migrate-and-seed"]
      clearDatabaseSetupState

isDatabaseSetupExecutable :: PostBuildComponentInputs -> Bool
isDatabaseSetupExecutable inputs =
  case targetComponent (targetInfo inputs) of
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
