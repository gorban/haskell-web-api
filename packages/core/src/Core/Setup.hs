-- | Shared setup utilities for Custom build-type packages.
--
-- Note: The core package's own Setup.hs cannot use this module
-- (it would create a circular dependency), so it duplicates this code.
module Core.Setup
  ( coreMain,
    copyLicenseFromRoot,
  )
where

import Control.Monad (when)
import Distribution.Simple (defaultMainWithHooks, preConf, simpleUserHooks)
import Distribution.Simple.Setup (BuildFlags, CommonSetupFlags (..), TestFlags, buildCommonFlags, emptyBuildFlags, testCabalFilePath, testDistPref, testTargets, testVerbosity, testWorkingDir)
import Distribution.Simple.UserHooks (buildHook, testHook)
import System.Directory (copyFile, doesFileExist)

-- | Main entry point for Custom Setup.hs files.
-- Runs the provided setup actions during the preConf phase.
--
-- Example:
--
-- @
-- main :: IO ()
-- main = coreMain [copyLicenseFromRoot]
-- @
coreMain :: [IO ()] -> IO ()
coreMain actions =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          sequence_ actions
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

-- | Copy LICENSE from the repository root if it doesn't exist locally.
copyLicenseFromRoot :: IO ()
copyLicenseFromRoot = do
  let src = "../../LICENSE"
      dest = "LICENSE"
  srcExists <- doesFileExist src
  destExists <- doesFileExist dest
  when (srcExists && not destExists) $ do
    copyFile src dest
    putStrLn "Setup: Copied LICENSE from repository root"
