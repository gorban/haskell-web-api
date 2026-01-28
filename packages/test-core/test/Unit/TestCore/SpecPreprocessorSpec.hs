{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT)
import Data.List (intercalate, isInfixOf)
import System.FilePath (takeFileName, (</>))
import TestCore.SpecPreprocessor (run, runPure)

spec = do
  describe "run" $ do
    it "fails with missing output argument" $ do
      result <- runExceptT $ run ["inputOnly.hs"]
      err <- $([|result|] `shouldMatch` [p|Left err|])
      err `shouldContain'` missingArgsError

    it "fails with missing input and output arguments" $ do
      result <- runExceptT $ run []
      err <- $([|result|] `shouldMatch` [p|Left err|])
      err `shouldContain'` missingArgsError

    it "fails with too many file arguments" $ do
      result <- runExceptT $ run ["file1.hs", "file2.hs", "file3.hs"]
      err <- $([|result|] `shouldMatch` [p|Left err|])
      err `shouldContain'` missingArgsError

    it "fails when input file is missing" $
      withTempFile "tst-missing" [] "MissingSpec.hs" $ \(_, missingFile) -> do
        result <- runExceptT $ run [missingFile, missingFile ++ ".out"]
        err <- $([|result|] `shouldMatch` [p|Left err|])
        err `shouldSatisfy` ("does not exist" `isInfixOf`)

    it "fails when output path is too long" $
      withExampleSpecTemp [] "LongOutputSpec" $ \(tempDir, tempFile) -> do
        writeFile tempFile pureSpecContents
        let longFileNameLength = 4000
            longFileName = replicate longFileNameLength 'o' ++ ".out"
            outputPath = tempDir </> longFileName
        result <- runExceptT $ run [tempFile, outputPath]
        err <- $([|result|] `shouldMatch` [p|Left err|])
        err `shouldSatisfy` ("invalid" `isInfixOf`)

    it "ignores files without SPEC pragma" $
      withExampleSpecTemp defaultModuleSegments exampleModuleBase $ \(_, tempFile) -> do
        let outputFile = getOutputFile tempFile
        writeFile tempFile "module Foo where"
        result <- runExceptT $ run [tempFile, outputFile]
        result `shouldBe` Right ()
        outputContents <- readFile outputFile
        _ <- evaluate (length outputContents)
        outputContents `shouldBe` "module Foo where\n"

    [ "{-# SPECC #-}",
      "}-# SPEC #-}",
      "{-# SPEC #-{",
      "{-# SPEC #-} extra"
      ]
      `forM_` \malformedPragma ->
        it ("ignores files with malformed SPEC pragma: " ++ show malformedPragma) $
          withExampleSpecTemp defaultModuleSegments exampleModuleBase $ \(_, tempFile) -> do
            let outputFile = getOutputFile tempFile
            writeFile tempFile malformedPragma
            result <- runExceptT $ run [tempFile, outputFile]
            result `shouldBe` Right ()
            outputContents <- readFile outputFile
            _ <- evaluate (length outputContents)
            outputContents `shouldBe` malformedPragma ++ "\n"

    around (withExampleSpecTemp defaultModuleSegments exampleModuleBase) $ do
      it "processes a simple spec file (default hs-source-dir)" $ \(_, tempFile) -> do
        let expectedHeader =
              getModuleHeader $
                getModuleName {- "test" -> empty segments: -} [] exampleModuleBase
            outputFile = getOutputFile tempFile
        writeFile tempFile "  {-#   SPEC   #-}  "
        result <- runExceptT $ run [tempFile, outputFile]
        result `shouldBe` Right ()
        outputContents <- readFile outputFile
        _ <- evaluate (length outputContents)
        outputContents `shouldContain'` expectedHeader

    around (withExampleSpecTemp nestedModuleSegments exampleModuleBase) $ do
      it "processes a simple spec file (2 file args)" $ \(tempDir, tempFile) -> do
        let expectedHeader = getModuleHeader $ getModuleName nestedModuleSegments exampleModuleBase
            hsSourceDir = takeFileName tempDir
            outputFile = getOutputFile tempFile
        writeFile tempFile "{-# SPEC   #-}  "
        result <- runExceptT $ run ["hs-source-dir=" ++ hsSourceDir, tempFile, outputFile]
        result `shouldBe` Right ()
        outputContents <- readFile outputFile
        _ <- evaluate (length outputContents)
        outputContents `shouldContain'` expectedHeader

    around (withExampleSpecTemp defaultModuleSegments exampleModuleBase) $ do
      it "processes a simple spec file (3 file args like GHC calls it)" $ \(tempDir, tempFile) -> do
        let expectedHeader = getModuleHeader $ getModuleName defaultModuleSegments exampleModuleBase
            hsSourceDir = takeFileName tempDir
            outputFile = getOutputFile tempFile
        writeFile tempFile "  {-#   SPEC #-}"
        result <- runExceptT $ run ["hs-source-dir=" ++ hsSourceDir, tempFile, tempFile, outputFile]
        result `shouldBe` Right ()
        outputContents <- readFile outputFile
        _ <- evaluate (length outputContents)
        outputContents `shouldContain'` expectedHeader

  describe "runPure" $ do
    it "processes a simple spec file" $ do
      let moduleBase = "PureSpec"
          hsRoot = "test-spec"
          inputPath = hsRoot </> getRelativePath nestedModuleSegments moduleBase
          absolutePath = "" </> "abs" </> inputPath
          expectedHeader = getModuleHeader (getModuleName nestedModuleSegments moduleBase)
          output = runPure hsRoot absolutePath pureSpecContents
      output `shouldContain'` expectedHeader

    [ ([], []),
      ([], ["-- comment"]),
      (["{-# LANGUAGE TemplateHaskell #-}"], ["-- comment"]),
      (["{-# LANGUAGE TemplateHaskell #-}", ""], [])
      ]
      `forM_` \(topSegments, importSegments) ->
        it
          ( "preserves existing imports and body (line "
              ++ show (length topSegments + length importSegments + 4)
              ++ ")"
          )
          $ do
            let moduleBase = "PureSpec"
                hsRoot = "test"
                inputPath = hsRoot </> getRelativePath nestedModuleSegments moduleBase
                absolutePath = "" </> "abs" </> inputPath
                contents =
                  unlines $
                    topSegments
                      ++ [ "{-# SPEC #-}",
                           "import Data.List (nub)"
                         ]
                      ++ importSegments
                      ++ [ "",
                           "spec = describe \"example\" $ do",
                           "  pure ()"
                         ]
                expectedFragments =
                  topSegments
                    ++ [ buildModuleHeader nestedModuleSegments moduleBase,
                         "",
                         "import TestCore.Prelude",
                         "import Data.List (nub)"
                       ]
                    ++ importSegments
                    ++ [ "",
                         "spec :: Spec",
                         ( "{-# LINE "
                             ++ show (length topSegments + length importSegments + 4)
                             ++ " \""
                             ++ map (\c -> if c == '\\' then '/' else c) absolutePath
                             ++ "\" #-}"
                         ),
                         "spec = describe \"example\" $ do",
                         "  pure ()"
                       ]
                output = runPure hsRoot absolutePath contents
             in lines output `shouldBe` expectedFragments

    it "infers modules when hs-source-dir is explicitly fallback value" $ do
      let moduleBase = "PureSpec"
          hsRoot = "test"
          inputPath = hsRoot </> getRelativePath nestedModuleSegments moduleBase
          absolutePath = "" </> "abs" </> inputPath
          expectedHeader = buildModuleHeader nestedModuleSegments moduleBase
          output = runPure hsRoot absolutePath pureSpecContents
      output `shouldContain'` expectedHeader

    it "infers modules using default hs-source-dir" $ do
      let moduleBase = "PureSpec"
          hsRoot = "test"
          inputPath = hsRoot </> getRelativePath nestedModuleSegments moduleBase
          absolutePath = "" </> "abs" </> inputPath
          expectedHeader = buildModuleHeader nestedModuleSegments moduleBase
          output = runPure "test" absolutePath pureSpecContents
      output `shouldContain'` expectedHeader

    it "infers modules for deeper nested directories" $ do
      let moduleSegments = ["Nested", "Deeper"]
          moduleBase = "DeepSpec"
          hsRoot = "test"
          inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
          absolutePath = "" </> "abs" </> inputPath
          expectedHeader = buildModuleHeader moduleSegments moduleBase
          output = runPure hsRoot absolutePath pureSpecContents
      output `shouldContain'` expectedHeader

    it "falls back to default module when absolute path empty" $ do
      let hsRoot = "test"
          output = runPure hsRoot "" pureSpecContents
      output `shouldContain'` getModuleHeader "Spec"

    it "falls back to default module when absolute path and source dir empty" $ do
      let output = runPure "" "" pureSpecContents
      output `shouldContain'` getModuleHeader "Spec"

    it "falls back to basename when hs-source-dir unmatched" $ do
      let moduleBase = "PureSpec"
          rootlessDir = "specs"
          inputPath = rootlessDir </> getRelativePath nestedModuleSegments moduleBase
          absolutePath = "" </> "abs" </> inputPath
          expectedHeader = getModuleHeader moduleBase
          output = runPure "test" absolutePath pureSpecContents
      output `shouldContain'` expectedHeader

    it "handles files without nested segments" $ do
      let moduleSegments = []
          moduleBase = "PureSpec"
          hsRoot = "test"
          inputPath = hsRoot </> getRelativePath moduleSegments moduleBase
          absolutePath = "" </> "abs" </> inputPath
          expectedHeader = getModuleHeader moduleBase
          output = runPure hsRoot absolutePath pureSpecContents
      output `shouldContain'` expectedHeader
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
    nestedModuleSegments = ["Nested"]
    defaultModuleSegments = ["test"]
    exampleModuleBase = "ExampleSpec"
