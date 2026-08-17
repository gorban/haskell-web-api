import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags, CommonSetupFlags (..), TestFlags, buildCommonFlags, emptyBuildFlags, testCabalFilePath, testDistPref, testTargets, testVerbosity, testWorkingDir)
import Distribution.Simple.UserHooks (buildHook, testHook)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { testHook = \args packageDescription localBuildInfo hooks flags -> do
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
