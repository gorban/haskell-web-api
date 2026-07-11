import Control.Monad (when)
import Distribution.Simple (defaultMainWithHooks, preConf, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags, CommonSetupFlags (..), TestFlags, buildCommonFlags, emptyBuildFlags, testCabalFilePath, testDistPref, testTargets, testVerbosity, testWorkingDir)
import Distribution.Simple.UserHooks (buildHook, testHook)
import System.Directory (copyFile, doesFileExist)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          copyLicenseFromRoot
          preConf simpleUserHooks args flags,
        testHook = \args packageDescription localBuildInfo hooks flags -> do
          buildHook simpleUserHooks packageDescription localBuildInfo hooks (buildFlagsFromTestFlags flags)
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

copyLicenseFromRoot :: IO ()
copyLicenseFromRoot = do
  let src = "../../LICENSE"
      dest = "LICENSE"
  srcExists <- doesFileExist src
  destExists <- doesFileExist dest
  when (srcExists && not destExists) $ do
    copyFile src dest
    putStrLn "Setup: Copied LICENSE from repository root"
