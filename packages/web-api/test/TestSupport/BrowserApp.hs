{-# LANGUAGE OverloadedStrings #-}

-- | Browser application fixtures use Hspec's existing scoped hooks: immutable
-- configuration and temporary assets may span the suite, and server lifetime is
-- selected explicitly with aroundAllWith or aroundWith. Browser sessions remain
-- owned by each runBrowserSpec; fixture sharing does not share client state.
module TestSupport.BrowserApp
  ( withBrowserApp,
    withBrowserServer,
  )
where

import HarchWeb qualified
import HarchWeb.OpenApi.Swagger (swaggerUiAssetsRoot)
import System.Directory (copyFile, createDirectory, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import TestCore.E2EPrelude (BrowserConfig, requirePlaywrightBrowserConfig)
import WebApi.AccountPages (AccountAction)
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)
import WebApi.Route (AppRoute)
import WebApi.Route qualified

-- | Adapt the existing bracketed server fixture to Hspec's inherited subject.
-- Use aroundAllWith for shared application variants, or aroundWith when each
-- example must own a fresh application lifetime. Exceptions retain the existing
-- bracket cleanup; this adapter does not catch failures or restart the server.
withBrowserServer :: (AppConfig -> HarchWeb.Application AppRoute AccountAction WebApi.Route.AppRequestContext WebApi.Route.AppAuthorization) -> ((BrowserConfig, HarchWeb.LocalTestServer) -> IO a) -> (BrowserConfig, AppConfig) -> IO a
withBrowserServer makeApplication action (browser, appConfig) =
  HarchWeb.withLocalTestServer (makeApplication appConfig) $ \server ->
    action (browser, server)

withBrowserApp :: ((BrowserConfig, AppConfig) -> IO a) -> IO a
withBrowserApp action = do
  swaggerAssetsRoot <- swaggerUiAssetsRoot "/docs/assets"
  browser <- requirePlaywrightBrowserConfig
  withSystemTempDirectory "web-api-e2e-assets" $ \assetDirectory ->
    do
      sourcePublicDirectory <- findSourcePublicDirectory
      copyDirectoryTree sourcePublicDirectory assetDirectory
      action
        ( browser,
          defaultAppConfig
            { staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = "/assets",
                            staticDirectory = assetDirectory
                          },
                        swaggerAssetsRoot
                      ],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  }
            }
        )

-- | Locate the package's @public@ asset directory by its stable base
-- stylesheet marker, walking up from the test working directory so the
-- fixture works from the package directory and the repository root alike.
findSourcePublicDirectory :: IO FilePath
findSourcePublicDirectory = getCurrentDirectory >>= searchFrom
  where
    searchFrom directory = do
      let candidates =
            [ directory </> "public",
              directory </> "packages/web-api/public"
            ]
      existing <- firstExisting candidates
      case existing of
        Just publicDirectory -> pure publicDirectory
        Nothing ->
          let parent = takeDirectory directory
           in if parent == directory
                then ioError (userError "could not locate packages/web-api/public")
                else searchFrom parent

    firstExisting paths =
      case paths of
        [] -> pure Nothing
        path : remaining -> do
          exists <- doesFileExist (path </> "styles" </> "app.css")
          if exists
            then pure (Just path)
            else firstExisting remaining

-- | Mirror one directory tree into an existing destination directory so the
-- temporary asset root serves every authored static file (base and
-- page-owned stylesheets alike) without touching the repository.
copyDirectoryTree :: FilePath -> FilePath -> IO ()
copyDirectoryTree source destination =
  listDirectory source >>= mapM_ copyEntry
  where
    copyEntry name = do
      let sourcePath = source </> name
          destinationPath = destination </> name
      isDirectory <- doesDirectoryExist sourcePath
      if isDirectory
        then do
          createDirectory destinationPath
          copyDirectoryTree sourcePath destinationPath
        else copyFile sourcePath destinationPath
