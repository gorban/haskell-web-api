{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

module SetupHooks (setupHooks) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Core.PageRoutes.Generator
  ( defaultGeneratorConfig,
    generatePageModules,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Distribution.Simple.SetupHooks
  ( BuildHooks (..),
    Component (..),
    ComponentName (..),
    Dependency (..),
    Dict (..),
    LibraryName (..),
    Location (..),
    PreBuildComponentInputs (..),
    PreBuildComponentRules,
    RulesM,
    SetupHooks (..),
    TargetInfo (..),
    addRuleMonitors,
    autogenComponentModulesDir,
    componentName,
    mkCommand,
    monitorDirectory,
    noBuildHooks,
    noSetupHooks,
    registerRule_,
    rules,
    staticRule,
  )
import Distribution.Utils.Path
  ( getSymbolicPath,
    makeRelativePathEx,
    makeSymbolicPath,
  )
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
    makeAbsolute,
  )
import System.FilePath (takeExtension, (</>))

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules = Just pageRouteRules
          }
    }

pageRouteRules :: PreBuildComponentRules
pageRouteRules =
  rules (static ()) $ \inputs ->
    case componentName (targetComponent (targetInfo inputs)) of
      CLibName LMainLibName -> registerPageRouteRule inputs
      _ -> pure ()

registerPageRouteRule :: PreBuildComponentInputs -> RulesM ()
registerPageRouteRule
  PreBuildComponentInputs
    { localBuildInfo = buildInfo,
      targetInfo = target
    } = do
  let pagesDirectory = "src/App/Pages"
      generatedDirectory =
        autogenComponentModulesDir
          buildInfo
          (targetCLBI target)
  (sourceDirectories, sourceFiles) <- liftIO (discoverPageInputs pagesDirectory)
  monitoredDirectories <- liftIO (traverse makeAbsolute sourceDirectories)
  -- Recompute the rule when a page is added to or removed from any discovered directory.
  addRuleMonitors (map monitorDirectory monitoredDirectories)
  registerRule_ "harch-page-routes" $
    staticRule
      ( mkCommand
          (static Dict)
          (static runPageGeneration)
          (pagesDirectory, getSymbolicPath generatedDirectory)
      )
      [ FileDependency
          (Location (makeSymbolicPath ".") (makeRelativePathEx sourceFile))
      | sourceFile <- sourceFiles
      ]
      ( Location generatedDirectory (makeRelativePathEx "App/Pages/Route/Generated.hs")
          :| [ Location generatedDirectory (makeRelativePathEx "App/Pages/Generated.hs"),
               Location generatedDirectory (makeRelativePathEx "harch-page-routes.manifest")
             ]
      )

runPageGeneration :: (FilePath, FilePath) -> IO ()
runPageGeneration (pagesDirectory, generatedDirectory) = do
  generationResult <-
    generatePageModules (defaultGeneratorConfig pagesDirectory generatedDirectory)
  either (ioError . userError . show) (const (pure ())) generationResult

discoverPageInputs :: FilePath -> IO ([FilePath], [FilePath])
discoverPageInputs directory = do
  entries <- listDirectory directory
  let paths = map (directory </>) entries
  directories <- filterM doesDirectoryExist paths
  files <- filterM doesFileExist paths
  nestedInputs <- traverse discoverPageInputs directories
  pure
    ( directory : concatMap fst nestedInputs,
      filter ((== ".hs") . takeExtension) files <> concatMap snd nestedInputs
    )
