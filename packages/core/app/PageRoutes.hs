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
    [pagesDirectory, generatedDirectory, applicationRouteModule, applicationRouteType, contextType] -> do
      let config =
            (defaultGeneratorConfig pagesDirectory generatedDirectory)
              { applicationRouteModuleName = applicationRouteModule,
                applicationRouteTypeName = applicationRouteType,
                requestContextTypeName = contextType
              }
      either (die . show) print =<< generatePageModules config
    _ ->
      die
        "usage: harch-page-routes PAGES_DIR GENERATED_DIR APP_ROUTE_MODULE APP_ROUTE_TYPE CONTEXT_TYPE"
