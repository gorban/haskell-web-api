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
import System.Directory (copyFile, createDirectory, doesFileExist, getCurrentDirectory)
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
withBrowserServer :: (AppConfig -> HarchWeb.Application AppRoute AccountAction WebApi.Route.AppRequestContext ()) -> ((BrowserConfig, HarchWeb.LocalTestServer) -> IO a) -> (BrowserConfig, AppConfig) -> IO a
withBrowserServer makeApplication action (browser, appConfig) =
  HarchWeb.withLocalTestServer (makeApplication appConfig) $ \server ->
    action (browser, server)

withBrowserApp :: ((BrowserConfig, AppConfig) -> IO a) -> IO a
withBrowserApp action = do
  browser <- requirePlaywrightBrowserConfig
  withSystemTempDirectory "web-api-e2e-assets" $ \assetDirectory ->
    do
      let stylesDirectory = assetDirectory </> "styles"
      createDirectory stylesDirectory
      sourceStylesheet <- findSourceStylesheet
      copyFile sourceStylesheet (stylesDirectory </> "app.css")
      action
        ( browser,
          defaultAppConfig
            { staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = "/assets",
                            staticDirectory = assetDirectory
                          }
                      ],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  }
            }
        )

findSourceStylesheet :: IO FilePath
findSourceStylesheet = getCurrentDirectory >>= searchFrom
  where
    searchFrom directory = do
      let candidates =
            [ directory </> "public/styles/app.css",
              directory </> "packages/web-api/public/styles/app.css"
            ]
      existing <- firstExisting candidates
      case existing of
        Just stylesheet -> pure stylesheet
        Nothing ->
          let parent = takeDirectory directory
           in if parent == directory
                then ioError (userError "could not locate packages/web-api/public/styles/app.css")
                else searchFrom parent

    firstExisting paths =
      case paths of
        [] -> pure Nothing
        path : remaining -> do
          exists <- doesFileExist path
          if exists
            then pure (Just path)
            else firstExisting remaining
