-- | Shared setup utilities for Custom build-type packages.
--
-- Note: The core package's own Setup.hs cannot use this module
-- (it would create a circular dependency), so it duplicates this code.
module Core.Setup
  ( coreMain,
  )
where

import Distribution.Simple (defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags, CommonSetupFlags (..), TestFlags, buildCommonFlags, emptyBuildFlags, testCabalFilePath, testDistPref, testTargets, testVerbosity, testWorkingDir)
import Distribution.Simple.UserHooks (buildHook, testHook)

-- | Main entry point for Custom Setup.hs files whose test hook must build an
-- internal executable before invoking the test suite.
coreMain :: IO ()
coreMain =
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
