-- | Generates conventional module wrappers for @{-# SPEC #-}@ test files.
--
-- This package is intentionally dependency-light.  In particular, it lets
-- @hspec-expectations-match@ use the same processor without creating the
-- package-level test cycle that results when a test tool depends on a package
-- that itself uses that matcher.  Standard specs default to
-- @TestCore.Prelude@; @spec-prelude=@ selects a different standard prelude.
-- @E2E_SPEC@ always imports @TestCore.E2EPrelude@.
--
-- The import section after the pragma may contain multi-line import lists:
-- this repository's formatter preserves src-style broken import lists, so a
-- continuation line is ordinary valid input and must never be mistaken for
-- the end of the imports (that would inject the generated @spec :: Spec@
-- signature into the middle of an import list).  See
-- 'processTillEndOfImports'.
--
-- Its direct Hspec suite is also this package's runtime coverage owner.  It
-- deliberately does not import the @TestCore@ compatibility facade: that
-- direction would recreate the package cycle this extraction avoids.
module TestSpecPreprocessor (run, runPure) where

import Control.Exception (IOException, displayException, try)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace)
import Data.List (intercalate, stripPrefix)
import System.Directory (makeAbsolute)
import System.FilePath (normalise, splitDirectories, takeBaseName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

data SpecMode = E2ESpec | StandardSpec

run :: [String] -> ExceptT String IO ()
run args = do
  let (hsSourceDir, standardPrelude, fileArgs) = parseArgs "test" "TestCore.Prelude" [] args
  case fileArgs of
    [input, output] -> processFile hsSourceDir standardPrelude input output
    [input, input', output]
      | input == input' -> processFile hsSourceDir standardPrelude input output
    _ -> throwError "spec-preprocessor: expected input and output file arguments"
  where
    processFile hsSourceDir standardPrelude input output = tryIO $ do
      absolutePath <- makeAbsolute input
      withFile input ReadMode $ \handle -> do
        contents <- hGetContents handle
        writeFile output $ runPureWithPrelude standardPrelude hsSourceDir absolutePath contents

    tryIO :: IO a -> ExceptT String IO a
    tryIO action = do
      result <- liftIO $ try action
      case result of
        Left (e :: IOException) -> throwError $ "spec-preprocessor: " ++ displayException e
        Right a -> pure a

runPure :: String -> String -> String -> String
runPure = runPureWithPrelude "TestCore.Prelude"

runPureWithPrelude :: String -> String -> String -> String -> String
runPureWithPrelude standardPrelude hsSourceDir absolutePath contents =
  unlines $ process 1 (inferModuleName hsSourceDir absolutePath) $ lines contents
  where
    process :: Int -> String -> [String] -> [String]
    process inputLine moduleName (header : rest) =
      let trimmed = dropWhile isSpace header
       in case stripSpecPragma trimmed of
            Just (specMode, remainder)
              | all isSpace remainder ->
                  let (importCount, imports, remaining) = processTillEndOfImports rest
                      originalLineOfRemaining = inputLine + 1 + importCount
                   in [ "module " ++ moduleName ++ " (spec) where",
                        "",
                        "import " ++ specPreludeModule standardPrelude specMode
                      ]
                        ++ imports
                        ++ [ "spec :: Spec",
                             "{-# LINE " ++ show originalLineOfRemaining ++ " \"" ++ normalizePath absolutePath ++ "\" #-}"
                           ]
                        ++ remaining
            _ -> header : process (inputLine + 1) moduleName rest
    process _ _ [] = []

stripSpecPragma :: String -> Maybe (SpecMode, String)
stripSpecPragma ('{' : '-' : '#' : xs) =
  let afterStart = dropWhile isSpace xs
   in case stripSpecMode afterStart of
        Just (specMode, rest') ->
          let afterSpec = dropWhile isSpace rest'
           in case afterSpec of
                '#' : '-' : '}' : r -> Just (specMode, r)
                _ -> Nothing
        Nothing -> Nothing
stripSpecPragma _ = Nothing

stripSpecMode :: String -> Maybe (SpecMode, String)
stripSpecMode ('E' : '2' : 'E' : '_' : 'S' : 'P' : 'E' : 'C' : rest) = Just (E2ESpec, rest)
stripSpecMode ('S' : 'P' : 'E' : 'C' : rest) = Just (StandardSpec, rest)
stripSpecMode _ = Nothing

specPreludeModule :: String -> SpecMode -> String
specPreludeModule _ E2ESpec = "TestCore.E2EPrelude"
specPreludeModule standardPrelude StandardSpec = standardPrelude

-- | Collect the contiguous import section that follows the spec pragma,
-- returning how many input lines it consumed, the collected lines in order,
-- and every remaining input line.
--
-- Multi-line import lists are ordinary valid Haskell in this repository
-- (the formatter preserves @src@-style broken import lists), so a
-- continuation line belongs to the section: any line still inside an
-- unbalanced import's parentheses continues it, as does an import-list
-- opener that sits alone on the line after its @import@ declaration.
-- Blank lines, comment lines, and new @import@ declarations continue the
-- section as before; any other line ends it, which is where the generated
-- @spec :: Spec@ signature belongs.  Previously every continuation line
-- ended the section, silently injecting the signature and a @LINE@ pragma
-- into the middle of the import list and producing an unrelated parse
-- error far from the real cause.
processTillEndOfImports :: [String] -> (Int, [String], [String])
processTillEndOfImports = go 0 0 []
  where
    go count parenDepth kept (line : rest)
      | continuesSection parenDepth line =
          go (count + 1) (advanceDepth parenDepth line) (line : kept) rest
      | otherwise = (count, reverse kept, line : rest)
    go count _parenDepth kept [] = (count, reverse kept, [])

    continuesSection parenDepth line
      | parenDepth > 0 = True
      | otherwise = startsImportListOperator || keep (dropWhile isSpace line)
      where
        startsImportListOperator = case dropWhile isSpace line of
          '(' : _ -> True
          _ -> False

    -- A comment is stripped before counting, so unbalanced parens in
    -- comment text (trailing or full-line) never shift the import-list
    -- depth; blanks naturally count zero.
    advanceDepth parenDepth line = parenDepth + parenDelta (stripLineComment line)

    -- Drop a Haskell line comment: a run of at least two dashes whose end
    -- is not a symbol character (or the end of the line).  Scanning one
    -- character past a rejected dash-run keeps operators such as @(--)@
    -- intact while still finding a later real comment.
    stripLineComment input@('-' : '-' : rest)
      | startsDashComment rest = []
      | otherwise = take 1 input ++ stripLineComment (drop 1 input)
    stripLineComment (character : rest) = character : stripLineComment rest
    stripLineComment [] = []

    startsDashComment rest = case rest of
      character : _ | isSymbolCharacter character -> False
      _ -> True

    isSymbolCharacter character =
      character
        `elem` ("!#$%&*+.,/:;<=>?@\\^|-~:" :: String)

    parenDelta line =
      length (filter (== '(') line) - length (filter (== ')') line)

    keep [] = True
    keep ('-' : '-' : _) = True
    keep ('i' : 'm' : 'p' : 'o' : 'r' : 't' : ' ' : _) = True
    keep _ = False

parseArgs :: String -> String -> [String] -> [String] -> (String, String, [String])
parseArgs hsSourceDir standardPrelude files [] = (hsSourceDir, standardPrelude, files)
parseArgs hsSourceDir standardPrelude files (arg : rest) =
  case stripPrefix "hs-source-dir=" arg of
    Just dir -> parseArgs dir standardPrelude files rest
    Nothing ->
      case stripPrefix "spec-prelude=" arg of
        Just preludeModule -> parseArgs hsSourceDir preludeModule files rest
        Nothing -> parseArgs hsSourceDir standardPrelude (files ++ [arg]) rest

inferModuleName :: String -> String -> String
inferModuleName hsSourceDir absolutePath =
  case absolutePath of
    [] ->
      let defaultModule = "Spec"
       in buildModuleName [] defaultModule
    _ ->
      let pathParts = splitDirectories $ normalise absolutePath
          baseName = takeBaseName absolutePath
       in case findModuleSegments pathParts hsSourceDir of
            Just segments -> buildModuleName segments baseName
            Nothing -> baseName
  where
    buildModuleName segments baseName = intercalate "." $ filter (not . null) (segments ++ [baseName])

findModuleSegments :: [String] -> String -> Maybe [String]
findModuleSegments pathParts sourceDir =
  let dirParts = take (max 0 (length pathParts - 1)) pathParts
      reversedDirs = reverse dirParts
      (between, after) = break (== sourceDir) reversedDirs
   in case after of
        [] -> Nothing
        _ : _ -> Just $ reverse between

normalizePath :: String -> String
normalizePath = map replaceBackslash
  where
    replaceBackslash '\\' = '/'
    replaceBackslash c = c
