{-# LANGUAGE TemplateHaskell #-}
{-# SPEC #-}

import Control.Exception (IOException, evaluate, finally, try)
import Data.Bifunctor (first)
import Data.List (intercalate)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (isDoesNotExistError)
import System.Mem (performGC)
import TestLib.SpecPreprocessor (run, runPure)
import TestLib.Temp (withTempFile)

spec = describe "Run spec-preprocessor" $ do
  it "Pure: processes a simple spec file" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test-spec"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        args = ["hs-source-dir=" ++ hsRoot, inputPath, "ignored.out"]
        expectedHeader = getModuleHeader (getModuleName moduleSegments moduleBase)
    output <- $([|runPure args absolutePath pureSpecContents|] `shouldMatch` [p|Right output|])
    output `shouldContain'` expectedHeader

  it "Fails with missing output argument" $ do
    err <- $([|runPure ["inputOnly.hs"] "/abs/inputOnly.hs" pureSpecContents|] `shouldMatch` [p|Left err|])
    err `shouldBe` missingArgsError

  it "Fails with missing input and output arguments" $ do
    err <- $([|runPure [] "/abs/ignored" pureSpecContents|] `shouldMatch` [p|Left err|])
    err `shouldBe` missingArgsError

  it "Preserves existing imports and body" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        args = ["hs-source-dir=" ++ hsRoot, inputPath, "ignored.out"]
        contents =
          unlines
            [ "{-# SPEC #-}",
              "import Data.List (nub)",
              "",
              "spec = describe \"example\" $ do",
              "  pure ()"
            ]
        expectedFragments =
          [ buildModuleHeader moduleSegments moduleBase,
            "import TestLib.Prelude",
            "import Data.List (nub)",
            "spec :: Spec",
            "spec = describe \"example\" $ do"
          ]
    output <- $([|runPure args absolutePath contents|] `shouldMatch` [p|Right output|])
    mapM_ (output `shouldContain'`) expectedFragments

  it "Infers modules when hs-source-dir is explicitly fallback value" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        args = ["hs-source-dir=" ++ hsRoot, inputPath, "ignored.out"]
        expectedHeader = buildModuleHeader moduleSegments moduleBase
    output <- $([|runPure args absolutePath pureSpecContents|] `shouldMatch` [p|Right output|])
    output `shouldContain'` expectedHeader

  it "Infers modules using default hs-source-dir" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        args = [inputPath, "ignored.out"]
        expectedHeader = buildModuleHeader moduleSegments moduleBase
    output <- $([|runPure args absolutePath pureSpecContents|] `shouldMatch` [p|Right output|])
    output `shouldContain'` expectedHeader

  it "Falls back to basename when hs-source-dir unmatched" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        rootlessDir = "specs"
        inputPath = rootlessDir </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        args = [inputPath, "ignored.out"]
        expectedHeader = getModuleHeader moduleBase
    output <- $([|runPure args absolutePath pureSpecContents|] `shouldMatch` [p|Right output|])
    output `shouldContain'` expectedHeader

  it "Handles files without nested segments" $ do
    let moduleSegments = []
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        args = ["hs-source-dir=" ++ hsRoot, inputPath, "ignored.out"]
        expectedHeader = getModuleHeader moduleBase
    output <- $([|runPure args absolutePath pureSpecContents|] `shouldMatch` [p|Right output|])
    output `shouldContain'` expectedHeader

  it "IO: Fails when input file is missing" $
    withTempFile "tst-missing" [] "MissingSpec.hs" $ \(_, missingFile) ->
      run [missingFile, missingFile ++ ".out"] `shouldThrow` isDoesNotExistError

  it "IO: Fails when output path is too long" $
    withExampleSpecTemp [] "LongOutputSpec" $ \(tempDir, tempFile) -> do
      writeFile tempFile pureSpecContents
      let longFileNameLength = 4000
          longFileName = replicate longFileNameLength 'o' ++ ".out"
          outputPath = tempDir </> longFileName
      (try (run [tempFile, outputPath]) :: IO (Either IOException ()))
        >>= \outcome -> $([|first (const ()) outcome|] `shouldMatch` [p|Left ()|])
        -- Force a GC cycle here to encourage timely release of file handles/resources
        -- after this filesystem error test (important on some platforms).
        `finally` performGC

  let moduleSegments = ["Nested"]
      moduleBase = "ExampleSpec"
  around (withExampleSpecTemp moduleSegments moduleBase) $ do
    it "IO: Processes a simple spec file" $ \(tempDir, tempFile) -> do
      let moduleSegments' = moduleSegments
          moduleBase' = moduleBase
          expectedHeader = getModuleHeader (getModuleName moduleSegments' moduleBase')
      writeFile tempFile "{-# SPEC #-}"
      let hsSourceDir = takeFileName tempDir
          outputFile = getOutputFile tempFile
      run ["hs-source-dir=" ++ hsSourceDir, tempFile, outputFile]
      outputContents <- readFile outputFile
      _ <- evaluate (length outputContents)
      outputContents `shouldContain'` expectedHeader
  where
    getHaskellName baseName = baseName ++ ".hs"
    getModuleName segments baseName = intercalate "." (segments ++ [baseName])
    getModuleHeader moduleName = "module " ++ moduleName ++ " (spec) where"
    buildModuleHeader segments baseName = getModuleHeader (getModuleName segments baseName)
    getRelativePath segments baseName = intercalate "/" (segments ++ [getHaskellName baseName])
    withExampleSpecTemp segments baseName = withTempFile "tst" segments (getHaskellName baseName)
    getOutputFile path = path ++ ".out"
    pureSpecContents = "{-# SPEC #-}"
    missingArgsError = "spec-preprocessor: expected input and output file arguments"
