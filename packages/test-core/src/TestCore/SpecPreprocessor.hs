module TestCore.SpecPreprocessor (run, runPure) where

import Control.Exception (IOException, displayException, try)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Core.System.Path (normalizePath)
import Data.Char (isSpace)
import Data.List (intercalate, stripPrefix)
import System.Directory (makeAbsolute)
import System.FilePath (normalise, splitDirectories, takeBaseName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

run :: [String] -> ExceptT String IO ()
run args = do
  let (hsSourceDir, fileArgs) = parseArgs "test" [] args
  case fileArgs of
    [input, output] ->
      processFile hsSourceDir input output
    -- GHC may pass the input file twice, we check to prevent an actual 3 positional argument call
    [input, input', output]
      | input == input' ->
          processFile hsSourceDir input output
    _ -> throwError "spec-preprocessor: expected input and output file arguments"
  where
    processFile hsSourceDir input output = tryIO $ do
      absolutePath <- makeAbsolute input
      withFile input ReadMode $ \handle -> do
        contents <- hGetContents handle
        writeFile output $ runPure hsSourceDir absolutePath contents

    tryIO :: IO a -> ExceptT String IO a
    tryIO action = do
      result <- liftIO $ try action
      case result of
        Left (e :: IOException) -> throwError $ "spec-preprocessor: " ++ displayException e
        Right a -> pure a

runPure :: String -> String -> String -> String
runPure hsSourceDir absolutePath contents =
  unlines $ process 1 (inferModuleName hsSourceDir absolutePath) $ lines contents
  where
    process :: Int -> String -> [String] -> [String]
    process inputLine moduleName (header : rest) =
      let trimmed = dropWhile isSpace header
       in case stripSpecPragma trimmed of
            Just remainder
              | all isSpace remainder ->
                  let (importCount, imports, remaining) = processTillEndOfImports rest
                      -- LINE pragma points to the first line of remaining content in the original file:
                      -- inputLine (current) + 1 (SPEC pragma) + importCount (imports we're copying)
                      originalLineOfRemaining = inputLine + 1 + importCount
                   in [ "module " ++ moduleName ++ " (spec) where",
                        "",
                        "import TestCore.Prelude"
                      ]
                        ++ imports
                        ++ [ "spec :: Spec",
                             "{-# LINE " ++ show originalLineOfRemaining ++ " \"" ++ normalizePath absolutePath ++ "\" #-}"
                           ]
                        ++ remaining
            _ -> header : process (inputLine + 1) moduleName rest
    -- Empty file case
    process _ _ [] = []

-- We are making sure that for the line with {-# SPEC #-}, at most the rest is whitespace
stripSpecPragma :: String -> Maybe String
stripSpecPragma ('{' : '-' : '#' : xs) =
  let afterStart = dropWhile isSpace xs
   in case afterStart of
        'S' : 'P' : 'E' : 'C' : rest' ->
          let afterSpec = dropWhile isSpace rest'
           in case afterSpec of
                '#' : '-' : '}' : r -> Just r
                _ -> Nothing
        _ -> Nothing
stripSpecPragma _ = Nothing

-- Process lines until we reach a line that is not an import or comment or empty
-- These are added after our added imports but before the spec definition
-- Returns (count, imports, remaining)
processTillEndOfImports :: [String] -> (Int, [String], [String])
processTillEndOfImports (header : rest) =
  let trimmed = dropWhile isSpace header
   in if keep trimmed
        then
          let (count, imports, remaining) = processTillEndOfImports rest
           in (count + 1, header : imports, remaining)
        else (0, [], header : rest)
  where
    -- If line is empty or whitespace,
    keep [] = True
    -- Or if first non-whitespace characters are for comment or import, add it and continue
    keep ('-' : '-' : _) = True
    keep ('i' : 'm' : 'p' : 'o' : 'r' : 't' : ' ' : _) = True
    -- Otherwise, stop processing imports
    keep _ = False
processTillEndOfImports [] = (0, [], [])

-- Parse arguments to extract hs-source-dir option and file arguments
-- Returns (hsSourceDir, fileArgs) where hsSourceDir defaults to "test"
parseArgs :: String -> [String] -> [String] -> (String, [String])
parseArgs hsSourceDir files [] = (hsSourceDir, files)
parseArgs hsSourceDir files (arg : rest) =
  case stripPrefix "hs-source-dir=" arg of
    Just dir -> parseArgs dir files rest
    Nothing -> parseArgs hsSourceDir (files ++ [arg]) rest

-- Infer module name from file path hierarchy
-- Goes up directories until hitting hs-source-dir
-- Returns module name as dotted namespace
inferModuleName :: String -> String -> String
inferModuleName hsSourceDir absolutePath =
  case absolutePath of
    [] ->
      let defaultModule = "Spec"
       in null hsSourceDir `seq` buildModuleName [] defaultModule
    _ ->
      let pathParts = splitDirectories $ normalise absolutePath
          baseName = takeBaseName absolutePath
       in case findModuleSegments pathParts hsSourceDir of
            Just segments -> buildModuleName segments baseName
            Nothing -> baseName
  where
    buildModuleName segments baseName =
      let parts = filter (not . null) (segments ++ [baseName])
       in intercalate "." parts

-- Find module segments by going up from file until hitting source directory
-- Returns Nothing if source directory is not found (fallback to basename)
findModuleSegments :: [String] -> String -> Maybe [String]
findModuleSegments pathParts sourceDir =
  let dirParts = take (max 0 (length pathParts - 1)) pathParts
      reversedDirs = reverse dirParts
      (between, after) = break (== sourceDir) reversedDirs
   in case after of
        [] -> Nothing
        _ : _ -> Just (reverse between)
