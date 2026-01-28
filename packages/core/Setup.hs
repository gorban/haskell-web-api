import Control.Monad (forM_, when)
import Distribution.Simple (defaultMainWithHooks, preConf, simpleUserHooks)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          copyLicenseFromRoot
          copyTestCoreSources
          preConf simpleUserHooks args flags
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

-- | Copy test-core sources locally to avoid relative paths outside source tree.
-- This allows core-tests to use the spec-preprocessor and TestCore.Prelude
-- without creating a package-level dependency cycle with test-core.
copyTestCoreSources :: IO ()
copyTestCoreSources = do
  let filesToCopy =
        [ ("../test-core/app/SpecPreprocessor.hs", "test-core-src/app/SpecPreprocessor.hs"),
          ("../test-core/src/TestCore/SpecPreprocessor.hs", "test-core-src/src/TestCore/SpecPreprocessor.hs"),
          ("../test-core/src/TestCore/Prelude.hs", "test-core-src/src/TestCore/Prelude.hs"),
          ("../test-core/src/TestCore/CustomAssertions.hs", "test-core-src/src/TestCore/CustomAssertions.hs")
        ]
  forM_ filesToCopy $ \(src, dest) -> do
    srcExists <- doesFileExist src
    when srcExists $ do
      createDirectoryIfMissing True (takeDirectory dest)
      copyFile src dest
  putStrLn "Setup: Copied test-core sources"
