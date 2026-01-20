{-# SPEC #-}

import Control.Exception (evaluate)
import Data.List (intercalate)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.Process (readProcessWithExitCode)
import TestLib.Temp (withTempFile)

spec = describe "Run spec-preprocessor" $ do
  it "Fails for missing arguments" $ do
    (exitCode, _, stderr) <- readProcessWithExitCode "spec-preprocessor" [] ""
    exitCode `shouldBe` ExitFailure 1
    stderr `shouldContain'` "spec-preprocessor: expected input and output file arguments"

  around withExampleSpecTemp $ do
    it "Processes a simple spec file" $ \(tempDir, tempFile) -> do
      writeFile tempFile "{-# SPEC #-}"
      let hsSourceDir = takeFileName tempDir
          outputFile = specOutputFile tempFile
      (exitCode, stdout, stderr) <- readProcessWithExitCode "spec-preprocessor" ["hs-source-dir=" ++ hsSourceDir, tempFile, outputFile] ""
      stdout `shouldBe` ""
      stderr `shouldBe` ""
      exitCode `shouldBe` ExitSuccess
      outputContents <- readFile outputFile
      _ <- evaluate (length outputContents)
      outputContents `shouldContain'` exampleModuleHeader
  where
    exampleModuleSegments = ["Nested"]
    exampleModuleBase = "ExampleSpec"
    exampleFileName = exampleModuleBase ++ ".hs"
    exampleModuleName = intercalate "." (exampleModuleSegments ++ [exampleModuleBase])
    exampleModuleHeader = "module " ++ exampleModuleName ++ " (spec) where"
    withExampleSpecTemp = withTempFile "tst" exampleModuleSegments exampleFileName
    specOutputFile path = path ++ ".out"
