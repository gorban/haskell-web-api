{-# SPEC #-}

import Control.Exception (evaluate)
import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty (NonEmpty ((:|)))
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.Process (readProcessWithExitCode)

spec = describe "main" $ do
  it "fails for missing arguments" $ do
    (exitCode, _, stderr) <- readProcessWithExitCode "spec-preprocessor" [] ""
    exitCode `shouldBe` ExitFailure 1
    stderr `shouldContain'` "spec-preprocessor: expected input and output file arguments"

  around withExampleSpecTemp $ do
    it "processes a simple spec file" $ \(tempDir, tempFile) -> do
      writeFile tempFile "{-# SPEC #-}"
      let hsSourceDir = takeFileName tempDir
          outputFile = specOutputFile tempFile
      (exitCode, stdout, stderr) <- readProcessWithExitCode "spec-preprocessor" ["hs-source-dir=" ++ hsSourceDir, tempFile, outputFile] ""
      expectAll
        ( (stdout `shouldBe` "")
            :| [ stderr `shouldSatisfy` isAcceptableSuccessfulStderr,
                 exitCode `shouldBe` ExitSuccess
               ]
        )
      outputContents <- readFile outputFile
      _ <- evaluate (length outputContents)
      expectAll
        ( (outputContents `shouldContain'` exampleModuleHeader)
            :| [outputContents `shouldContain'` "import TestCore.Prelude"]
        )

    it "processes a simple e2e spec file" $ \(tempDir, tempFile) -> do
      writeFile tempFile "{-# E2E_SPEC #-}"
      let hsSourceDir = takeFileName tempDir
          outputFile = specOutputFile tempFile
      (exitCode, stdout, stderr) <- readProcessWithExitCode "spec-preprocessor" ["hs-source-dir=" ++ hsSourceDir, tempFile, outputFile] ""
      expectAll
        ( (stdout `shouldBe` "")
            :| [ stderr `shouldSatisfy` isAcceptableSuccessfulStderr,
                 exitCode `shouldBe` ExitSuccess
               ]
        )
      outputContents <- readFile outputFile
      _ <- evaluate (length outputContents)
      expectAll
        ( (outputContents `shouldContain'` exampleModuleHeader)
            :| [outputContents `shouldContain'` "import TestCore.E2EPrelude"]
        )
  where
    exampleModuleSegments = ["Nested"]
    exampleModuleBase = "ExampleSpec"
    exampleFileName = exampleModuleBase ++ ".hs"
    exampleModuleName = intercalate "." (exampleModuleSegments ++ [exampleModuleBase])
    exampleModuleHeader = "module " ++ exampleModuleName ++ " (spec) where"
    withExampleSpecTemp = withTempFile "tst" exampleModuleSegments exampleFileName
    specOutputFile path = path ++ ".out"
    isAcceptableSuccessfulStderr stderrOutput =
      null stderrOutput || hpcDeprecationWarningPrefix `isPrefixOf` stderrOutput
    hpcDeprecationWarningPrefix =
      unlines
        [ "Deprecation warning:",
          "I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.",
          "GHC 9.14 will cease looking for an existing tix file by default."
        ]
