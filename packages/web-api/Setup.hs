import Control.Monad (when)
import Core.Setup.PrerequisiteReport
  ( DatabasePrerequisiteStatus (DatabasePrerequisiteAutostarted),
    SetupPrerequisiteReport (databasePrerequisiteStatus),
    reportSetupPrerequisitesAndReturn,
  )
import Distribution.Simple (defaultMainWithHooks, preConf, simpleUserHooks)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.LocalBuildInfo (buildDir)
import Distribution.Simple.Setup (BuildFlags, CommonSetupFlags (..), TestFlags, buildCommonFlags, emptyBuildFlags, testCabalFilePath, testDistPref, testTargets, testVerbosity, testWorkingDir)
import Distribution.Simple.UserHooks (buildHook, testHook)
import Distribution.Types.LocalBuildInfo (LocalBuildInfo, hostPlatform)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory (doesFileExist, removeFile)
import System.FilePath ((</>))
import System.Process (callProcess)

main :: IO ()
main = do
  let runBuildAndDatabaseSetup packageDescription localBuildInfo hooks flags = do
        buildHook simpleUserHooks packageDescription localBuildInfo hooks flags
        runDatabaseSetupIfNeeded localBuildInfo
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          recordDatabaseAutostart
          preConf simpleUserHooks args flags,
        buildHook = runBuildAndDatabaseSetup,
        testHook = \args packageDescription localBuildInfo hooks flags -> do
          runBuildAndDatabaseSetup packageDescription localBuildInfo hooks (buildFlagsFromTestFlags flags)
          testHook simpleUserHooks args packageDescription localBuildInfo hooks flags
      }

buildFlagsFromTestFlags :: TestFlags -> BuildFlags
buildFlagsFromTestFlags flags =
  emptyBuildFlags
    { buildCommonFlags =
        CommonSetupFlags
          { setupVerbosity = testVerbosity flags,
            setupWorkingDir = testWorkingDir flags,
            setupDistPref = testDistPref flags,
            setupCabalFilePath = testCabalFilePath flags,
            setupTargets = testTargets flags,
            setupKeepTempFiles = mempty
          }
    }

recordDatabaseAutostart :: IO ()
recordDatabaseAutostart = do
  reportedPrerequisites <- reportSetupPrerequisitesAndReturn
  if databaseWasAutostarted reportedPrerequisites
    then writeFile databaseSetupStatePath "autostarted\n"
    else clearDatabaseSetupState

databaseWasAutostarted :: Either loadError SetupPrerequisiteReport -> Bool
databaseWasAutostarted reportedPrerequisites =
  case reportedPrerequisites of
    Right prerequisiteReport ->
      case databasePrerequisiteStatus prerequisiteReport of
        DatabasePrerequisiteAutostarted _ _ -> True
        _ -> False
    Left _ -> False

runDatabaseSetupIfNeeded :: LocalBuildInfo -> IO ()
runDatabaseSetupIfNeeded localBuildInfo = do
  setupStateExists <- doesFileExist databaseSetupStatePath
  when setupStateExists $ do
    putStrLn "Setup: Running database migrations and seed data via haskell-web-api-db."
    callProcess (builtExecutablePath localBuildInfo "haskell-web-api-db") ["migrate-and-seed"]
    clearDatabaseSetupState

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
