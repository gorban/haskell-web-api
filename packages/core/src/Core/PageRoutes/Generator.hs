{-# LANGUAGE OverloadedStrings #-}

module Core.PageRoutes.Generator
  ( GenerationError (..),
    GenerationOutcome (..),
    GeneratorConfig (..),
    PageSpec (..),
    defaultGeneratorConfig,
    discoverPages,
    generatePageModules,
    pageSpecFromRelativePath,
    renderDispatcherModule,
    renderManifest,
    renderRouteModule,
    validatePageSpecs,
  )
where

import Control.Monad (filterM, forM)
import Data.Bits (xor)
import Data.Char (isAlphaNum, isUpper, ord, toLower)
import Data.List (intercalate, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Word (Word64)
import Numeric (showHex)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath
  ( dropExtension,
    joinPath,
    makeRelative,
    splitDirectories,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )

data GeneratorConfig = GeneratorConfig
  { pagesSourceDirectory :: FilePath,
    generatedSourceDirectory :: FilePath,
    -- | The module namespace every discovered page module lives under, with its
    -- trailing dot: a page file @Home.hs@ becomes @<prefix>Home@ and
    -- @Account/Login.hs@ becomes @<prefix>Account.Login@.  It is configuration
    -- rather than a constant because the namespace belongs to the application,
    -- not to this tool: a framework build step has no business deciding that
    -- every application's pages are called @App.Pages.@.
    pageModulePrefix :: String,
    -- | The application context type each generated @pageDefinition@ takes, when
    -- the application's page handlers need runtime services.
    --
    -- A page definition is a value, and an application whose handler is a
    -- function of its configuration cannot express one without a parameter:
    -- the handler needs the value that carries those services.  When this is
    -- @Just \"AppContext\"@ the generated dispatcher becomes
    -- @pageRouteDefinition :: AppContext -> PageRoute -> RouteDefinition …@
    -- and delegates @\<Page\>.pageDefinition context@; when it is 'Nothing' —
    -- the default — the generated shape is unchanged, so every existing caller
    -- keeps working.
    pageDefinitionContextTypeName :: Maybe String,
    -- | The module the context type lives in, so the generated dispatcher can
    -- import it.  A generated module has no other way to bring the type into
    -- scope, and a qualified name would only move the same fact into a string
    -- that has to be split apart again.  Set it together with
    -- 'pageDefinitionContextTypeName'; leaving both unset keeps the
    -- parameterless dispatcher.
    pageDefinitionContextModuleName :: Maybe String,
    routeModuleName :: String,
    dispatcherModuleName :: String,
    applicationRouteModuleName :: String,
    applicationRouteTypeName :: String,
    requestContextTypeName :: String,
    authorizationTypeName :: String
  }
  deriving (Eq, Show)

defaultGeneratorConfig :: FilePath -> FilePath -> GeneratorConfig
defaultGeneratorConfig pagesDirectory generatedDirectory =
  GeneratorConfig
    { pagesSourceDirectory = pagesDirectory,
      generatedSourceDirectory = generatedDirectory,
      pageModulePrefix = "App.Pages.",
      pageDefinitionContextTypeName = Nothing,
      pageDefinitionContextModuleName = Nothing,
      routeModuleName = "App.Pages.Route.Generated",
      dispatcherModuleName = "App.Pages.Generated",
      applicationRouteModuleName = "App.Routes",
      applicationRouteTypeName = "TwoPageRoute",
      requestContextTypeName = "()",
      authorizationTypeName = "()"
    }

data PageSpec = PageSpec
  { pageSourcePath :: FilePath,
    pageConstructor :: String,
    pageModuleName :: String,
    pageUrlPath :: Text,
    pageSourceHash :: String
  }
  deriving (Eq, Show)

data GenerationError
  = PagesDirectoryMissing FilePath
  | NoPagesDiscovered FilePath
  | InvalidPageModulePrefix String
  | InvalidPagePath FilePath
  | MissingPageDefinition FilePath
  | ConstructorCollision String [FilePath]
  | PathCollision Text [FilePath]
  deriving (Eq, Show)

data GenerationOutcome
  = Generated [FilePath]
  | Unchanged [FilePath]
  deriving (Eq, Show)

generatePageModules :: GeneratorConfig -> IO (Either GenerationError GenerationOutcome)
generatePageModules config = do
  discovered <- discoverPages (pageModulePrefix config) (pagesSourceDirectory config)
  case discovered of
    Left generationError -> pure (Left generationError)
    Right pageSpecs -> do
      let routePath = moduleOutputPath config (routeModuleName config)
          dispatcherPath = moduleOutputPath config (dispatcherModuleName config)
          manifestPath = generatedSourceDirectory config </> "harch-page-routes.manifest"
          outputs =
            [ (routePath, renderRouteModule config pageSpecs),
              (dispatcherPath, renderDispatcherModule config pageSpecs),
              (manifestPath, renderManifest pageSpecs)
            ]
      changed <- or <$> traverse (uncurry writeIfChanged) outputs
      pure (Right ((if changed then Generated else Unchanged) (map fst outputs)))

-- | Discover the page modules under a directory, naming each one under the
-- supplied module prefix.  The prefix is the application's, so it is an argument
-- rather than a constant (see 'pageModulePrefix').
discoverPages :: String -> FilePath -> IO (Either GenerationError [PageSpec])
discoverPages pageModulePrefix sourceDirectory = do
  if not (validPageModulePrefix pageModulePrefix)
    then pure (Left (InvalidPageModulePrefix pageModulePrefix))
    else do
      sourceExists <- doesDirectoryExist sourceDirectory
      if not sourceExists
        then pure (Left (PagesDirectoryMissing sourceDirectory))
        else do
          sourceFiles <- listHaskellFiles sourceDirectory
          case sourceFiles of
            [] -> pure (Left (NoPagesDiscovered sourceDirectory))
            _ -> do
              pageResults <-
                forM sourceFiles $ \sourcePath -> do
                  source <- readFile sourcePath
                  let relativePath = makeRelative sourceDirectory sourcePath
                  pure (pageSpecFromRelativePath pageModulePrefix relativePath source)
              pure $ do
                pageSpecs <- sequenceA pageResults
                validatePageSpecs pageSpecs

pageSpecFromRelativePath :: String -> FilePath -> String -> Either GenerationError PageSpec
pageSpecFromRelativePath pageModulePrefix relativePath source = do
  let pathSegments = splitDirectories (dropExtension relativePath)
  if not (validPageModulePrefix pageModulePrefix)
    then Left (InvalidPageModulePrefix pageModulePrefix)
    else
      if takeExtension relativePath /= ".hs"
        || takeFileName relativePath == ".hs"
        || null pathSegments
        || not (all validModuleSegment pathSegments)
        then Left (InvalidPagePath relativePath)
        else
          if not (hasPageDefinition source)
            then Left (MissingPageDefinition relativePath)
            else
              Right
                PageSpec
                  { pageSourcePath = relativePath,
                    pageConstructor = constructorFor pathSegments,
                    pageModuleName = pageModulePrefix <> intercalate "." pathSegments,
                    pageUrlPath = pathFor pathSegments,
                    pageSourceHash = stableHash source
                  }

validatePageSpecs :: [PageSpec] -> Either GenerationError [PageSpec]
validatePageSpecs pageSpecs = do
  constructorChecked <-
    rejectCollision
      (ConstructorCollision . Text.unpack)
      (Text.pack . pageConstructor)
      pageSpecs
  pathChecked <- rejectCollision PathCollision pageUrlPath constructorChecked
  pure (sortOn pageSourcePath pathChecked)

renderRouteModule :: GeneratorConfig -> [PageSpec] -> String
renderRouteModule config pageSpecs =
  unlines
    ( [ "{-# LANGUAGE OverloadedStrings #-}",
        "",
        "module " <> routeModuleName config,
        "  ( PageRoute (..),",
        "    allPageRoutes,",
        "    pageRoutePath,",
        "    parsePageRoute,",
        "  )",
        "where",
        "",
        "import Data.Text (Text)",
        "",
        "data PageRoute"
      ]
        <> renderConstructors pageSpecs
        <> [ "  deriving (Bounded, Enum, Eq, Show)",
             "",
             "allPageRoutes :: [PageRoute]",
             "allPageRoutes = [minBound .. maxBound]",
             "",
             "pageRoutePath :: PageRoute -> Text",
             "pageRoutePath route =",
             "  case route of"
           ]
        <> map (renderRouteCase (Text.pack . show . Text.unpack . pageUrlPath)) pageSpecs
        <> [ "",
             "parsePageRoute :: Text -> Maybe PageRoute",
             "parsePageRoute path =",
             "  case path of"
           ]
        <> map renderParseCase pageSpecs
        <> ["    _ -> Nothing"]
    )

renderDispatcherModule :: GeneratorConfig -> [PageSpec] -> String
renderDispatcherModule config pageSpecs =
  unlines
    ( [ "module " <> dispatcherModuleName config,
        "  ( pageRouteDefinition,",
        "  )",
        "where",
        "",
        "import "
          <> applicationRouteModuleName config
          <> " ("
          <> intercalate ", " (filter (/= "()") [applicationRouteTypeName config, requestContextTypeName config, authorizationTypeName config])
          <> ")",
        "import " <> routeModuleName config <> " (PageRoute (..))",
        "import HarchWeb.Site (RouteDefinition)"
      ]
        <> contextImport
        <> map (\pageSpec -> "import " <> pageModuleName pageSpec <> " qualified") pageSpecs
        <> [ "",
             "pageRouteDefinition :: "
               <> contextParameterType
               <> "PageRoute -> RouteDefinition "
               <> applicationRouteTypeName config
               <> " "
               <> requestContextTypeName config
               <> " "
               <> authorizationTypeName config,
             "pageRouteDefinition " <> contextParameterName <> "route =",
             "  case route of"
           ]
        <> map (renderRouteCase (renderPageDefinition config)) pageSpecs
    )
  where
    contextParameterType = maybe "" (<> " -> ") (pageDefinitionContextTypeName config)
    contextParameterName = maybe "" (const "context ") (pageDefinitionContextTypeName config)
    contextImport =
      [ "import " <> contextModule <> " (" <> contextType <> ")"
      | Just contextModule <- [pageDefinitionContextModuleName config],
        Just contextType <- [pageDefinitionContextTypeName config]
      ]

-- | How the dispatcher reaches a page's own definition.  With a declared
-- context type the page receives it; without one the definition is a plain
-- value, which is the shape most applications want.
renderPageDefinition :: GeneratorConfig -> PageSpec -> Text
renderPageDefinition config pageSpec =
  Text.pack (pageModuleName pageSpec <> ".pageDefinition")
    <> maybe "" (const " context") (pageDefinitionContextTypeName config)

renderManifest :: [PageSpec] -> String
renderManifest pageSpecs =
  unlines
    [pageSourcePath pageSpec <> "\t" <> pageSourceHash pageSpec | pageSpec <- sortOn pageSourcePath pageSpecs]

renderConstructors :: [PageSpec] -> [String]
renderConstructors pageSpecs =
  case pageSpecs of
    [] -> ["  = NoPagesGenerated"]
    firstPage : remainingPages ->
      ("  = " <> pageConstructor firstPage)
        : map (("  | " <>) . pageConstructor) remainingPages

renderRouteCase :: (PageSpec -> Text) -> PageSpec -> String
renderRouteCase renderValue pageSpec =
  "    " <> pageConstructor pageSpec <> " -> " <> Text.unpack (renderValue pageSpec)

renderParseCase :: PageSpec -> String
renderParseCase pageSpec =
  "    \"" <> Text.unpack (pageUrlPath pageSpec) <> "\" -> Just " <> pageConstructor pageSpec

constructorFor :: [String] -> String
constructorFor ["NotFound"] = "PageNotFound"
constructorFor segments = concat segments <> "Page"

pathFor :: [String] -> Text
pathFor ["Home"] = "/"
pathFor ["NotFound"] = "/404"
pathFor segments = "/" <> Text.intercalate "/" (map kebabCase segments)

kebabCase :: String -> Text
kebabCase value =
  Text.drop 1 (Text.concat (map renderCharacter value))
  where
    renderCharacter character
      | isUpper character =
          Text.cons
            '-'
            (Text.singleton (toLower character))
      | otherwise = Text.singleton character

validModuleSegment :: String -> Bool
validModuleSegment segment =
  let segmentText = Text.pack segment
      firstCharacter = Text.take 1 segmentText
   in not (Text.null firstCharacter)
        && Text.all isUpper firstCharacter
        && Text.all isAlphaNum segmentText

validPageModulePrefix :: String -> Bool
validPageModulePrefix pageModulePrefix =
  case reverse pageModulePrefix of
    '.' : reversedModuleName ->
      let moduleSegments = splitOn '.' (reverse reversedModuleName)
       in not (null moduleSegments) && all validModuleSegment moduleSegments
    _ -> False

hasPageDefinition :: String -> Bool
hasPageDefinition source =
  any definesPageDefinition (lines source)
  where
    definesPageDefinition sourceLine =
      case Text.stripPrefix "pageDefinition" (Text.stripStart (Text.pack sourceLine)) of
        Just remainder ->
          let definitionTail = Text.stripStart remainder
           in "::" `Text.isPrefixOf` definitionTail || "=" `Text.isPrefixOf` definitionTail
        Nothing -> False

rejectCollision ::
  (Text -> [FilePath] -> GenerationError) ->
  (PageSpec -> Text) ->
  [PageSpec] ->
  Either GenerationError [PageSpec]
rejectCollision makeError key pageSpecs =
  case Map.toAscList (Map.filter ((> 1) . length) groupedPages) of
    (duplicate, collision) : _ ->
      Left (makeError duplicate (map pageSourcePath (sortOn pageSourcePath collision)))
    [] -> Right pageSpecs
  where
    groupedPages = Map.fromListWith (<>) [(key pageSpec, [pageSpec]) | pageSpec <- pageSpecs]

listHaskellFiles :: FilePath -> IO [FilePath]
listHaskellFiles directory = do
  entries <- sort <$> listDirectory directory
  let paths = map (directory </>) entries
  directories <- filterM doesDirectoryExist paths
  files <- filterM doesFileExist paths
  nestedFiles <- concat <$> traverse listHaskellFiles directories
  pure (sort (filter ((== ".hs") . takeExtension) files <> nestedFiles))

moduleOutputPath :: GeneratorConfig -> String -> FilePath
moduleOutputPath config moduleName =
  generatedSourceDirectory config </> joinPath (splitOn '.' moduleName) <> ".hs"

writeIfChanged :: FilePath -> String -> IO Bool
writeIfChanged outputPath output = do
  existing <- doesFileExist outputPath
  existingOutput <- if existing then Just <$> TextIO.readFile outputPath else pure Nothing
  let renderedOutput = Text.pack output
  if existingOutput == Just renderedOutput
    then pure False
    else do
      createDirectoryIfMissing True (takeDirectory outputPath)
      TextIO.writeFile outputPath renderedOutput
      pure True

stableHash :: String -> String
stableHash source =
  let hashValue = foldl' updateHash 14695981039346656037 source
   in showHex hashValue ""
  where
    updateHash :: Word64 -> Char -> Word64
    updateHash hashValue character = (hashValue `xor` fromIntegral (ord character)) * 1099511628211

splitOn :: (Eq value) => value -> [value] -> [[value]]
splitOn separator values =
  case break (== separator) values of
    (segment, []) -> [segment]
    (segment, _ : remaining) -> segment : splitOn separator remaining
