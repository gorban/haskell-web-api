module TestLib.SpecPreprocessor (run, runPure) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate, isSuffixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import System.Directory (makeAbsolute)
import System.Exit (die)

run :: [String] -> IO ()
run args =
  case parseRunArgs args of
    Left err -> die err
    Right (opts, input, output) -> do
      contents <- readFile input
      absolutePath <- makeAbsolute input
      let result = transform opts input absolutePath contents
      writeFile output result

runPure :: [String] -> FilePath -> String -> Either String String
runPure args absoluteInputPath contents =
  case parseRunArgs args of
    Left err -> Left err
    Right (opts, input, _) -> Right (transform opts input absoluteInputPath contents)

parseRunArgs :: [String] -> Either String (Options, FilePath, FilePath)
parseRunArgs args =
  case reverse args of
    (output : input : restRev) ->
      Right (parseOptions (reverse restRev), input, output)
    _ -> Left missingArgsMessage

missingArgsMessage :: String
missingArgsMessage = "spec-preprocessor: expected input and output file arguments"

data Options where
  Options :: {sourceRoots :: [String]} -> Options

defaultOptions :: Options
defaultOptions = Options {sourceRoots = ["test"]}

parseOptions :: [String] -> Options
parseOptions rawArgs =
  case mapMaybe parseHsSourceDir rawArgs of
    [] -> defaultOptions
    dirs -> Options {sourceRoots = dirs}
  where
    parseHsSourceDir = stripPrefix "hs-source-dir="

transform :: Options -> FilePath -> FilePath -> String -> String
transform opts path absolutePath contents =
  case analyze contents of
    Nothing -> contents
    Just (pragmas, markerLine, body, bodyStartLine) ->
      let moduleName = resolveModuleName opts markerLine path
          (importsSectionRaw, remainderRaw) = splitImportSection body
          importsSection = trimTrailingBlankLines importsSectionRaw
          importsWithPrelude = ensurePrelude importsSection
          (remainderLeadingBlanks, remainderBody) = span isBlank remainderRaw
          remainder = remainderBody
          specSection = ["spec :: Spec"]
          specLine = bodyStartLine + length importsSectionRaw + length remainderLeadingBlanks
          linePragma = mkLinePragma specLine (normalizePath absolutePath)
          remainderWithLine = case remainder of
            [] -> []
            _ -> linePragma : remainder
          importsBlock = case importsWithPrelude of
            [] -> []
            _ -> importsWithPrelude ++ [""]
          finalLines =
            pragmas
              ++ ["module " ++ moduleName ++ " (spec) where"]
              ++ importsBlock
              ++ specSection
              ++ remainderWithLine
       in unlines (trimTrailingBlankLines finalLines)

newtype Marker = Marker (Maybe String)

analyze :: String -> Maybe ([String], Marker, [String], Int)
analyze contents =
  let ls = map stripCarriage (lines contents)
      (leadingPragmas, rest) = span leadingPragma ls
   in case rest of
        (line : restLines)
          | Just marker <- parseMarker line ->
              let (followingPragmas, body0) = span isPragma restLines
                  body = body0
                  bodyStartLine = length leadingPragmas + 1 + length followingPragmas + 1
               in Just (leadingPragmas ++ followingPragmas, marker, body, bodyStartLine)
        _ -> Nothing
  where
    leadingPragma line =
      isPragma line && case parseMarker line of
        Nothing -> True
        Just _ -> False

isPragma :: String -> Bool
isPragma line =
  let trimmed = dropWhile isSpace line
   in "{-#" `isPrefixOf` trimmed

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = prefix == take (length prefix) str

isBlank :: String -> Bool
isBlank = all isSpace

parseMarker :: String -> Maybe Marker
parseMarker line =
  let trimmed = dropWhile isSpace line
   in case parsePragmaMarker trimmed of
        Just marker -> Just marker
        Nothing -> parseCommentMarker trimmed

parsePragmaMarker :: String -> Maybe Marker
parsePragmaMarker trimmed
  | "{-#" `isPrefixOf` trimmed && "#-}" `isSuffixOf` trimmed =
      let innerWithTrailing = drop 3 trimmed
          inner = take (length innerWithTrailing - 3) innerWithTrailing
          stripped = dropWhile isSpace inner
       in case words stripped of
            ("SPEC" : rest) -> parseSpecArgs rest
            _ -> Nothing
  | otherwise = Nothing

parseSpecArgs :: [String] -> Maybe Marker
parseSpecArgs [] = Just (Marker Nothing)
parseSpecArgs [token]
  | Just name <- stripPrefix "module=" token = Just (Marker (Just name))
parseSpecArgs _ = Nothing

parseCommentMarker :: String -> Maybe Marker
parseCommentMarker trimmed =
  case words trimmed of
    ["--", "%spec"] -> Just (Marker Nothing)
    ("--" : "%spec" : moduleName : _) -> Just (Marker (Just moduleName))
    _ -> Nothing

resolveModuleName :: Options -> Marker -> FilePath -> String
resolveModuleName _ (Marker (Just name)) _ = name
resolveModuleName opts (Marker Nothing) path =
  case moduleFromPath opts path of
    Just name -> name
    Nothing -> error ("spec-preprocessor: unable to infer module name from path: " ++ path)

moduleFromPath :: Options -> FilePath -> Maybe String
moduleFromPath opts path =
  let normalized = map replaceSlash path
      segments = split '/' normalized
      moduleSegments = remainderFromRoots segments (sourceRoots opts)
   in case moduleSegments of
        Just segs@(_ : _) -> Just (intercalate "." (init segs ++ [dropHsSuffix (last segs)]))
        _ -> fallbackFromPath segments

replaceSlash :: Char -> Char
replaceSlash '\\' = '/'
replaceSlash c = c

split :: Char -> String -> [String]
split delim = foldr step [[]]
  where
    step c acc@(x : xs)
      | c == delim = [] : acc
      | otherwise = (c : x) : xs
    step _ [] = [[]]

dropHsSuffix :: String -> String
dropHsSuffix str
  | ".hs" `isSuffixOf` str = take (length str - 3) str
  | otherwise = str

splitImportSection :: [String] -> ([String], [String])
splitImportSection = go []
  where
    go acc [] = (reverse acc, [])
    go acc (line : rest)
      | isBlank line = go (line : acc) rest
      | isImport line = go (line : acc) rest
      | otherwise = (reverse acc, line : rest)

isImport :: String -> Bool
isImport line =
  let trimmed = dropWhile isSpace line
   in "import" `isPrefixOf` trimmed

ensurePrelude :: [String] -> [String]
ensurePrelude imports
  | any (containsPrelude . dropWhile isSpace) imports = imports
  | otherwise =
      let (leadingBlanks, rest) = span isBlank imports
       in case rest of
            [] -> leadingBlanks ++ ["import TestLib.Prelude"]
            _ -> leadingBlanks ++ ("import TestLib.Prelude" : rest)

containsPrelude :: String -> Bool
containsPrelude line = "import TestLib.Prelude" `isPrefixOf` line

mkLinePragma :: Int -> FilePath -> String
mkLinePragma lineNumber filePath = "{-# LINE " ++ show lineNumber ++ " \"" ++ filePath ++ "\" #-}"

normalizePath :: FilePath -> FilePath
normalizePath = map replaceSlash

trimTrailingBlankLines :: [String] -> [String]
trimTrailingBlankLines = reverse . dropWhile isBlank . reverse

stripCarriage :: String -> String
stripCarriage = dropWhileEnd (== '\r')

remainderFromRoots :: [String] -> [String] -> Maybe [String]
remainderFromRoots segments = go
  where
    go [] = Nothing
    go (root : rest) =
      case matchRoot segments (splitRoot root) of
        Just remainder -> Just remainder
        Nothing -> go rest

    matchRoot :: [String] -> [String] -> Maybe [String]
    matchRoot segs [] = Just segs
    matchRoot segs rootSegs =
      case stripPrefix rootSegs segs of
        Just remainder -> Just remainder
        Nothing ->
          case segs of
            [] -> Nothing
            (_ : xs) -> matchRoot xs rootSegs

    splitRoot :: String -> [String]
    splitRoot rootPath = filter (not . null) (split '/' (map replaceSlash rootPath))

fallbackFromPath :: [String] -> Maybe String
fallbackFromPath segments =
  case reverse segments of
    [] -> Nothing
    (file : _) | null file -> Nothing
    (file : _) -> Just (dropHsSuffix file)
