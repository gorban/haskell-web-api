module Main (main) where

import Core.PageRoutes.Generator
  ( GeneratorConfig (..),
    defaultGeneratorConfig,
    generatePageModules,
  )
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    [pagesDirectory, generatedDirectory, applicationRouteModule, applicationRouteType, contextType] ->
      runGeneration (defaultGeneratorConfig pagesDirectory generatedDirectory) applicationRouteModule applicationRouteType contextType
    -- The page module namespace is the application's, so it can be supplied
    -- rather than assumed; omitting it keeps the historical App.Pages default.
    [pagesDirectory, generatedDirectory, applicationRouteModule, applicationRouteType, contextType, pageModulePrefix] ->
      runGeneration
        ((defaultGeneratorConfig pagesDirectory generatedDirectory) {pageModulePrefix = pageModulePrefix})
        applicationRouteModule
        applicationRouteType
        contextType
    _ ->
      die
        "usage: harch-page-routes PAGES_DIR GENERATED_DIR APP_ROUTE_MODULE APP_ROUTE_TYPE CONTEXT_TYPE [PAGE_MODULE_PREFIX]"

runGeneration :: GeneratorConfig -> String -> String -> String -> IO ()
runGeneration baseConfig applicationRouteModule applicationRouteType contextType = do
  let config =
        baseConfig
          { applicationRouteModuleName = applicationRouteModule,
            applicationRouteTypeName = applicationRouteType,
            requestContextTypeName = contextType
          }
  either (die . show) print =<< generatePageModules config
