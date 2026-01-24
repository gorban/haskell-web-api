{-# LANGUAGE TemplateHaskell #-}
{-# SPEC #-}

import Control.Exception (evaluate)
import Control.Monad.Except (runExceptT)
import Data.List (intercalate, isInfixOf)
import System.FilePath (takeFileName, (</>))
import TestLib.SpecPreprocessor (run, runPure)
import TestLib.Temp (withTempFile)

spec = describe "Run spec-preprocessor" $ do
  it "Pure: processes a simple spec file" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test-spec"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        expectedHeader = getModuleHeader (getModuleName moduleSegments moduleBase)
        output = runPure hsRoot absolutePath pureSpecContents
    output `shouldContain'` expectedHeader

  it "IO: Fails with missing output argument" $ do
    result <- runExceptT $ run ["inputOnly.hs"]
    err <- $([|result|] `shouldMatch` [p|Left err|])
    err `shouldContain'` missingArgsError

  it "IO: Fails with missing input and output arguments" $ do
    result <- runExceptT $ run []
    err <- $([|result|] `shouldMatch` [p|Left err|])
    err `shouldContain'` missingArgsError

  it "IO: Fails with too many file arguments" $ do
    result <- runExceptT $ run ["file1.hs", "file2.hs", "file3.hs"]
    err <- $([|result|] `shouldMatch` [p|Left err|])
    err `shouldContain'` missingArgsError

  it "Preserves existing imports and body" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
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
        output = runPure hsRoot absolutePath contents
    mapM_ (output `shouldContain'`) expectedFragments

  it "Infers modules when hs-source-dir is explicitly fallback value" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        expectedHeader = buildModuleHeader moduleSegments moduleBase
        output = runPure hsRoot absolutePath pureSpecContents
    output `shouldContain'` expectedHeader

  it "Infers modules using default hs-source-dir" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        expectedHeader = buildModuleHeader moduleSegments moduleBase
        output = runPure "test" absolutePath pureSpecContents
    output `shouldContain'` expectedHeader

  it "Falls back to basename when hs-source-dir unmatched" $ do
    let moduleSegments = ["Nested"]
        moduleBase = "PureSpec"
        rootlessDir = "specs"
        inputPath = rootlessDir </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        expectedHeader = getModuleHeader moduleBase
        output = runPure "test" absolutePath pureSpecContents
    output `shouldContain'` expectedHeader

  it "Handles files without nested segments" $ do
    let moduleSegments = []
        moduleBase = "PureSpec"
        hsRoot = "test"
        inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
        absolutePath = "" </> "abs" </> inputPath
        expectedHeader = getModuleHeader moduleBase
        output = runPure hsRoot absolutePath pureSpecContents
    output `shouldContain'` expectedHeader

  it "IO: Fails when input file is missing" $
    withTempFile "tst-missing" [] "MissingSpec.hs" $ \(_, missingFile) -> do
      result <- runExceptT $ run [missingFile, missingFile ++ ".out"]
      err <- $([|result|] `shouldMatch` [p|Left err|])
      err `shouldSatisfy` ("does not exist" `isInfixOf`)

  it "IO: Fails when output path is too long" $
    withExampleSpecTemp [] "LongOutputSpec" $ \(tempDir, tempFile) -> do
      writeFile tempFile pureSpecContents
      let longFileNameLength = 4000
          longFileName = replicate longFileNameLength 'o' ++ ".out"
          outputPath = tempDir </> longFileName
      result <- runExceptT $ run [tempFile, outputPath]
      err <- $([|result|] `shouldMatch` [p|Left err|])
      err `shouldSatisfy` ("invalid" `isInfixOf`)

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
      result <- runExceptT $ run ["hs-source-dir=" ++ hsSourceDir, tempFile, outputFile]
      result `shouldBe` Right ()
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
